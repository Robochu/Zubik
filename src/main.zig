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

    const cube_set = try zubik.CubeSet.init(allocator, 2 << 24);
    defer cube_set.deinit(allocator);
    var time = timer.read();
    std.debug.print("Initialization completed in {d}ms.\n", .{ @as(f64, @floatFromInt(time)) / std.time.ns_per_ms });

    var cube = zubik.solved_cube.applyTurnString("D2 B' R' D2 L U D' R L2 D2 B' U2 F D2 R2 D2 L2 F' D2 L'") catch unreachable;
    var solution: [zubik.gods_number * 10]zubik.Turn = undefined;

    timer.reset();
    const depth = zubik.qsolve(cube, &solution);
    time = timer.read();
    std.debug.print("zubik.solve() finished in {d} seconds.\n", .{@as(f64, @floatFromInt(time)) / std.time.ns_per_s});
    for (solution[0..depth]) |turn| {
        std.debug.print("{any}\n", .{turn});
    }
    std.debug.print("\n", .{});
}
