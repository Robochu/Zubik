const std = @import("std");
const zubik = @import("zubik.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer if (gpa.deinit() == .leak) {
        @panic("Memory leak detected.");
    };

    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.os.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const rand = prng.random();
    _ = rand;

    var timer = try std.time.Timer.start();
    try zubik.initTurnTables(allocator, "turn_tables.cache");
    defer zubik.deinitTurnTables(allocator);

    try zubik.initPartialTables(allocator, "partial_tables.cache");
    defer zubik.deinitPartialTables(allocator);

    try zubik.initInstantTables(allocator, "instant_tables.cache");
    defer zubik.deinitInstantTables(allocator);

    var time = timer.read();
    std.debug.print("Initialization completed in {d}ms.\n", .{ @as(f64, @floatFromInt(time)) / std.time.ns_per_ms });

    var cube = zubik.solved_cube.applyTurnString("D2 B' U2 B2 F L2 B U2 L2 R F' U2 R B' F2 U L U") catch unreachable;
    var solution: [zubik.gods_number * 10]zubik.Turn = undefined;
    _ = solution;

    timer.reset();
    const sol = zubik.solveInstant(cube);
    time = timer.read();
    std.debug.print("zubik.solveInstant() finished in {d} seconds.\n", .{@as(f64, @floatFromInt(time)) / std.time.ns_per_s});
    var len: usize = 0;
    for (sol) |turns| {
        len += turns.len;
        for (turns) |turn| {
            std.debug.print("{any}\n", .{turn});
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("{d}\n", .{len});
    std.debug.print("\n", .{});
}
