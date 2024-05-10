const std = @import("std");
const stdout_file = std.io.getStdOut().writer();
var stdout_bw = std.io.bufferedWriter(stdout_file);
const stdout = stdout_bw.writer();

// const stdin_file = std.io.getStdOut().writer();
// var stdout_bw = std.io.bufferedWriter(stdout_file);
// const stdout = stdout_bw.writer();
const stdin = std.io.getStdIn().reader();

const Mark = enum {
    X,
    O,
    Nil,
    pub fn format(
        self: Mark,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll(switch (self) {
            .X => "X",
            .O => "O",
            .Nil => " ",
        });
    }
};

const TTTColumnIter = struct {
    idx: u64 = 0,
    parent: *const TicTacToe,
    fn next(self: *@This()) ?Mark {
        const data: []const Mark = self.parent.data;
        const side_len = self.parent.side_len;
        defer self.idx += 1;
        if (self.idx < data.len) {
            return data[(self.idx % side_len) * side_len];
        } else {
            return null;
        }
    }
};

const TTTRowIter = struct {
    idx: u64 = 0,
    parent: *const TicTacToe,
    fn next(self: *@This()) ?Mark {
        defer self.idx += 1;
        if (self.idx < self.parent.data.len) {
            return self.parent.data[self.idx];
        } else {
            return null;
        }
    }
};

const TTTDiagonalIter = struct {
    idx: u64 = 0,
    parent: *const TicTacToe,
    fn next(self: *@This()) ?Mark {
        const data: []const Mark = self.parent.data;
        const side_len = self.parent.side_len;
        defer self.idx += 1;
        if (self.idx < side_len) {
            // Diagonal starting at top left and ending in bottom right
            return data[self.idx * side_len + self.idx];
        } else if (self.idx < 2 * side_len) {
            // Diagonal starting at top right and ending in bottom left
            const sub_idx = self.idx - side_len;
            return data[sub_idx * side_len + side_len - sub_idx - 1];
        } else {
            return null;
        }
    }
};

const TicTacToe = struct {
    side_len: u32,
    data: []Mark,

    // Interaction methods
    /// `move` expects to be passed an array of 0-indexed integers representing moves
    ///  0 | 1 | 2
    /// ---+---+---
    ///  3 | 4 | 5
    /// ---+---+---
    ///  6 | 7 | 8
    fn move(self: *TicTacToe, drill: []const u8, mark: Mark) void {
        std.debug.assert(drill.len == 1);
        self.data[drill[0]] = mark;
    }
    fn show(self: *const TicTacToe) !void {
        try stdout.print(" {} | {} | {}\n", .{ self.data[0], self.data[1], self.data[2] });
        try stdout.print("---+---+---\n", .{});
        try stdout.print(" {} | {} | {}\n", .{ self.data[3], self.data[4], self.data[5] });
        try stdout.print("---+---+---\n", .{});
        try stdout.print(" {} | {} | {}\n", .{ self.data[6], self.data[7], self.data[8] });
        try stdout.print("\n", .{});
        try stdout_bw.flush(); // don't forget to flush!
    }

    // Iterator methods
    fn rows(self: *const TicTacToe) TTTRowIter {
        return TTTRowIter{ .parent = self };
    }
    fn cols(self: *const TicTacToe) TTTColumnIter {
        return TTTColumnIter{ .parent = self };
    }
    fn diags(self: *const TicTacToe) TTTDiagonalIter {
        return TTTDiagonalIter{ .parent = self };
    }
};

fn ttt_new(side_len: u32, allocator: std.mem.Allocator) !TicTacToe {
    const data: []Mark = try allocator.alloc(Mark, side_len * side_len);
    return TicTacToe{ .side_len = side_len, .data = data };
}

fn ttt_free(p: *const TicTacToe, allocator: std.mem.Allocator) void {
    allocator.free(p.data);
}

/// If a slice of marks are all the same, return that mark. If there's no matching mark return .Nil.
/// Expects an iterator that returns `Mark`s as the first param, and the length of rows/columns/diagonals as the second param.
fn all_same(mark_iter: anytype, run_length: u32) Mark {
    var target = Mark.Nil;
    var run_idx: u32 = 0;
    while (mark_iter.next()) |mark| {
        if (run_idx >= run_length) {
            break;
        }
        run_idx += 1;
        if (target == .Nil) {
            target = mark;
        } else if (target != mark) {
            return .Nil;
        }
    }
    return target;
}

fn winner(g: TicTacToe) Mark {
    // Check for winning rows
    var row_iter = g.rows();
    for (0..g.side_len) |row_idx| {
        const winning_mark = all_same(&row_iter, g.side_len);
        if (winning_mark != .Nil) {
            std.debug.print("Won on row {d}.\n", .{row_idx});
            return winning_mark;
        }
    }

    // Check for winning columns
    var col_iter = g.cols();
    for (0..g.side_len) |col_idx| {
        const winning_mark = all_same(&col_iter, g.side_len);
        if (winning_mark != .Nil) {
            std.debug.print("Won on col {d}.\n", .{col_idx});
            return winning_mark;
        }
    }

    // Check for winning diagonals
    var diag_iter = g.diags();
    for (0..2) |diag_idx| {
        const winning_mark = all_same(&diag_iter, g.side_len);
        if (winning_mark != .Nil) {
            std.debug.print("Won on diagonal {d}.\n", .{diag_idx});
            return winning_mark;
        }
    }
    return .Nil;
}

fn gauntlet(g: TicTacToe) void {
    var row_iter = g.rows();
    while (row_iter.next()) |row| {
        std.log.info("got row {}", .{std.json.fmt(row, .{})});
    }
    var col_iter = g.cols();
    while (col_iter.next()) |col| {
        std.log.info("got col {}", .{std.json.fmt(col, .{})});
    }
    var diag_iter = g.diags();
    while (diag_iter.next()) |diag| {
        std.log.info("got diags {}", .{std.json.fmt(diag, .{})});
    }
    std.log.info("checking winner: {}", .{std.json.fmt(winner(g), .{})});
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    const alloc = std.heap.c_allocator;

    var g = try ttt_new(4, alloc);
    defer ttt_free(&g, alloc);

    // Main game loop
    try g.show();
    var current_mark: Mark = .O;
    var winning_mark: Mark = .Nil;
    while (winning_mark == .Nil) {
        current_mark = switch (current_mark) {
            .X => .O,
            .O => .X,
            .Nil => .Nil,
        };
        try stdout.print("It's your turn, {}!\n", .{current_mark});
        try stdout_bw.flush();

        var in_buf = [_]u8{0} ** 256;
        var in_buf_stream = std.io.fixedBufferStream(&in_buf);
        try stdin.streamUntilDelimiter(in_buf_stream.writer(), '\n', 256);
        const move_choice = try std.fmt.charToDigit(in_buf[0], 10);
        g.move(&[1]u8{move_choice}, current_mark);
        try g.show();

        winning_mark = winner(g);
    }
    try stdout.print("You win, {}! Good job!\n", .{winning_mark});
    try stdout_bw.flush();
    // I'm bored, fuck Zig, there are 3 hours left, I'm so sleepy..... uwu
}
