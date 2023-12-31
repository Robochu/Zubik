//! TODO
//! TODO HTM
//!
//! The representation of a Rubik's cube is first split into two parts: orientation and permutation. Orientation
//! dictates how the individual pieces are rotated. In particular, an edge may either be correctly oriented or flipped,
//! while still remaining in the same position relative to the cube's center. Similarly, a corner can be either be
//! oriented correctly, rotated clockwise, or rotated counterclockwise, all while remaining in the same place.
//! Permutation describes where the pieces are relative to the cube's center. Since a 3x3 has fixed center pieces, this
//! is sufficient to uniquely describe a Rubik's cube.
//!
//! Orientation and permutation are further split into edges and corners. Finally, permutation of edges is split into
//! 3 parts, each part containing the permutation of 4 out of 12 edges. The final representation therefore has 6 parts.
//! Each individual part is small enough that it can be directly represented using a 16-bit integer. Turning is done
//! using turn tables, which map a turn + one of the 6 parts back to the same part after the turn was performed.
//! Generating turn tables requires quite a bit of code, but once they are generated making a turn is as simple as 6
//! table accesses.
//! TODO references
//! TODO guides
//! TODO check references in doccomments

const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

///////////////////////
// Utility functions //
///////////////////////

inline fn factorial(comptime n: comptime_int) comptime_int {
    comptime assert(n >= 0);
    return if (n == 0) 1 else n * factorial(n - 1);
}

test "factorial" {
    try testing.expectEqual(1, factorial(0));
    try testing.expectEqual(1, factorial(1));
    try testing.expectEqual(2, factorial(2));
    try testing.expectEqual(6, factorial(3));
    try testing.expectEqual(24, factorial(4));
    try testing.expectEqual(120, factorial(5));
}

fn reverse(array: anytype) [array.len]@typeInfo(@typeInfo(@TypeOf(array)).Pointer.child).Array.child {
    comptime assert(array.len <= std.math.maxInt(u8));
    var array_reversed: [array.len]@typeInfo(@typeInfo(@TypeOf(array)).Pointer.child).Array.child = undefined;
    var i: u8 = 0;
    while (i < array.len) : (i += 1) {
        array_reversed[array_reversed.len - 1 - i] = array[i];
    }
    return array_reversed;
}

test "reverse" {
    try testing.expectEqual([_]u8{}, reverse(&[_]u8{}));
    try testing.expectEqual([_]u8{1}, reverse(&[_]u8{1}));
    try testing.expectEqual([_]u8{ 2, 1 }, reverse(&[_]u8{ 1, 2 }));
    try testing.expectEqual([_]u8{ 3, 2, 1 }, reverse(&[_]u8{ 1, 2, 3 }));
    try testing.expectEqual([_]u8{ 3, 2, 2, 1 }, reverse(&[_]u8{ 1, 2, 2, 3 }));
    try testing.expectEqual([_]u8{ 6, 4, 7, 2, 1 }, reverse(&[_]u8{ 1, 2, 7, 4, 6 }));
}

fn countOccurences(array: anytype, item: anytype) usize {
    var count: usize = 0;
    for (array) |array_item| {
        if (array_item == item) {
            count += 1;
        }
    }
    return count;
}

test "countOccurences" {
    try testing.expectEqual(@as(usize, 3), countOccurences(&[_]u8{ 2, 3, 3, 4, 5, 3 }, 3));
    try testing.expectEqual(@as(usize, 4), countOccurences("a  b  c", ' '));
    try testing.expectEqual(@as(usize, 0), countOccurences("aoeu", 'i'));
}

fn parity(permutation_array: anytype) bool {
    comptime assert(permutation_array.len <= std.math.maxInt(u8));
    var parity_count: u8 = 0;
    var visited = [_]bool{false} ** permutation_array.len;
    var i: u8 = 0;
    while (i < permutation_array.len) : (i += 1) {
        if (!visited[i]) {
            visited[i] = true;
            var cycle_len: u8 = 1;
            var j = permutation_array[i];
            while (j != i) : (j = permutation_array[j]) {
                visited[j] = true;
                cycle_len += 1;
            }
            parity_count += (cycle_len - 1) % 2;
        }
    }
    return parity_count % 2 == 1;
}

test "parity" {
    try testing.expectEqual(false, parity([_]u8{ 0, 1, 2, 3, 4, 5, 6, 7 }));
    try testing.expectEqual(true, parity([_]u8{ 0, 1, 2, 3, 4, 5, 7, 6 }));
    try testing.expectEqual(true, parity([_]u8{ 3, 7, 6, 0, 4, 5, 2, 1 }));
    try testing.expectEqual(false, parity([_]u8{ 2, 1, 5, 3, 4, 0, 6, 7 }));
    try testing.expectEqual(true, parity([_]u8{ 4, 2, 1, 5, 0, 7, 3, 6 }));
    try testing.expectEqual(false, parity([_]u8{ 6, 7, 0, 4, 1, 2, 5, 3 }));
}

///////////////
// Constants //
///////////////

pub const turns_per_face = 3;
pub const faces_per_axis = 2;
pub const turns_per_axis = turns_per_face * faces_per_axis;
pub const edges_per_face = 4;
pub const corners_per_face = 4;

pub const edges_parts = 3;
pub const total_parts = 6;

pub const face_count = 6;
pub const turn_count = face_count * turns_per_face;
pub const edge_count = 12;
pub const partial_edge_count = edge_count / edges_parts;
pub const corner_count = 8;
pub const piece_count = edge_count + corner_count;
pub const partial_table_count = total_parts * (total_parts - 1) / 2;

pub const edge_orientation_count = 2;
pub const corner_orientation_count = 3;

pub const gods_number = 20;
pub const valid_state_count = edges_orientation_count * corners_orientation_count * edges_permutation_count *
    corners_permutation_count / 2;
// TODO doccomments

/////////////////////
// Data structures //
/////////////////////

// The following enums all have the restriction that they have to start at 0 and go up by 1 so that they can be used for
// indexing. Turn has an extra restriction that the 18 turns have to be grouped by face in multiples of 3 and by axis in
// multiples of 6 in order to be able to quickly avoid performing same-face turns twice in a row and same-axis turns
// thrice in a row. The groups must be in the same order as Face. EdgeOrientation and CornerOrientation cannot be
// changed at all.
pub const Face = enum(u8) { U, D, R, L, F, B };
pub const Turn = enum(u8) { U, @"U'", U2, D, @"D'", D2, R, @"R'", R2, L, @"L'", L2, F, @"F'", F2, B, @"B'", B2 };
pub const Edge = enum(u8) { UR, UL, UF, UB, DR, DL, DF, DB, RF, RB, LF, LB };
pub const Corner = enum(u8) { URF, URB, ULF, ULB, DRF, DRB, DLF, DLB };
pub const EdgeOrientation = enum(u8) { correct, flipped };
pub const CornerOrientation = enum(u8) { correct, clockwise, counterclockwise };

pub const face_edges_clockwise = [face_count][edges_per_face]Edge{
    .{ .UB, .UR, .UF, .UL },
    .{ .DF, .DR, .DB, .DL },
    .{ .UR, .RB, .DR, .RF },
    .{ .UL, .LF, .DL, .LB },
    .{ .UF, .RF, .DF, .LF },
    .{ .UB, .LB, .DB, .RB },
};

pub const face_corners_clockwise = [face_count][corners_per_face]Corner{
    .{ .ULB, .URB, .URF, .ULF },
    .{ .DLF, .DRF, .DRB, .DLB },
    .{ .URF, .URB, .DRB, .DRF },
    .{ .ULB, .ULF, .DLF, .DLB },
    .{ .ULF, .URF, .DRF, .DLF },
    .{ .URB, .ULB, .DLB, .DRB },
};

/// `turn_names[@enumToInt(turn)] == @tagName(turn)`. Used to convert a sequence of turns into a readable string.
pub const turn_names = blk: {
    var names: [turn_count][]const u8 = undefined;
    for (std.enums.values(Turn)) |turn| {
        names[@intFromEnum(turn)] = @tagName(turn);
    }
    break :blk names;
};

/// Inverse of `zubik.turn_names`. Used to convert a readable string into a sequence of turns.
pub const turn_map = blk: {
    var kvs_list: [turn_count]struct { []const u8, Turn } = undefined;
    for (std.enums.values(Turn)) |turn| {
        kvs_list[@intFromEnum(turn)] = .{ @tagName(turn), turn };
    }
    break :blk std.ComptimeStringMap(Turn, kvs_list);
};

//
// TODO
//

/// Convert a string of turns like "R U R' U' R' F R2 U' R' U' R U R' F'" into an array of turns. Returns a subslice of
/// `turns` on success, `error.InvalidTurnString` if the string contains an invalid turn, and `error.OutOfBounds` if
/// `turns` isn't long enough to hold all of the turns. A safe length for `turns` is `turn_string.len / 2 + 1`. See also
/// `allocTurnsFromString`, `stringFromTurn`, `allocStringFromTurn`, and `Cube.applyTurnString`.
pub fn turnsFromString(turn_string: []const u8, turns: []Turn) ![]Turn {
    if (turn_string.len == 0) {
        return turns[0..0];
    }
    
    var i: usize = 0;
    var it = std.mem.splitScalar(u8, turn_string, ' ');
    while (it.next()) |turn_name| {
        if (i == turns.len) {
            return error.OutOfBounds;
        } else if (turn_map.get(turn_name)) |turn| {
            turns[i] = turn;
            i += 1;
        } else {
            return error.InvalidTurnString;
        }
    }
    return turns[0..i];
}

test "turnsFromString" {
    var turns: [5]Turn = undefined;
    try testing.expectEqualSlices(Turn, &.{}, try turnsFromString("", &turns));
    try testing.expectEqualSlices(Turn, &.{ .R, .U, .@"R'", .@"U'" }, try turnsFromString("R U R' U'", &turns));
    try testing.expectEqualSlices(Turn, &.{ .L2, .R2, .D2, .R2, .L2 }, try turnsFromString("L2 R2 D2 R2 L2", &turns));
    try testing.expectError(error.InvalidTurnString, turnsFromString("F R U I T", &turns));
    try testing.expectError(error.OutOfBounds, turnsFromString("B' D F2 D2 F' D'", &turns));
}

/// Same as `turnsFromString`, but with the allocation done for you. The returned slice has to be freed.
pub fn allocTurnsFromString(allocator: std.mem.Allocator, turn_string: []const u8) ![]Turn {
    if (turn_string.len == 0) {
        return &.{};
    }

    const turns = try allocator.alloc(Turn, countOccurences(turn_string, ' ') + 1);
    const turns_copy = turnsFromString(turn_string, turns) catch |err| {
        assert(err != error.OutOfBounds);
        allocator.free(turns);
        return err;
    };
    assert(turns_copy.ptr == turns.ptr and turns_copy.len == turns.len);
    return turns;
}

test "allocTurnsFromString" {
    const expected = .{
        .{},
        .{ .R, .U, .B },
        .{ .F, .U, .R },
    };

    const actual = .{
        try allocTurnsFromString(testing.allocator, ""),
        try allocTurnsFromString(testing.allocator, "R U B"),
        try allocTurnsFromString(testing.allocator, "F U R"),
    };
    defer inline for (actual) |turns| {
        testing.allocator.free(turns);
    };

    inline for (expected, actual) |expected_turns, actual_turns| {
        try testing.expectEqualSlices(Turn, &expected_turns, actual_turns);
    }
    try testing.expectError(error.InvalidTurnString, allocTurnsFromString(testing.allocator, "R Y"));
}

/// Convert a slice of turns into a readable string. Returns a subslice of `turn_string` on success and
/// `error.OutOfBounds` if `turn_string` isn't long enough. A safe length for `turn_string` is `turns.len * 3 - 1`. See
/// also `allocStringFromTurns`, `turnsFromString`, and `allocTurnsFromString`.
pub fn stringFromTurns(turns: []const Turn, turn_string: []u8) ![]u8 {
    var i: usize = 0;
    for (turns, 0..) |turn, j| {
        const turn_name = turn_names[@intFromEnum(turn)];
        if (turn_string.len < i + turn_name.len) {
            return error.OutOfBounds;
        }
        @memcpy(turn_string[i..(i + turn_name.len)], turn_name);
        i += turn_name.len;
        if (j < turns.len - 1) {
            if (turn_string.len <= i) {
                return error.OutOfBounds;
            }
            turn_string[i] = ' ';
            i += 1;
        }
    }
    return turn_string[0..i];
}

test "stringFromTurns" {
    var buffer: [10]u8 = undefined;
    var turns: [5]Turn = undefined;
    try testing.expectEqualStrings("", try stringFromTurns(&.{}, &buffer));
    try testing.expectEqualStrings("D'", try stringFromTurns(&.{.@"D'"}, &buffer));
    try testing.expectEqualStrings("F' U' B2", try stringFromTurns(&.{ .@"F'", .@"U'", .B2 }, &buffer));
    try testing.expectEqualStrings("L' B D' R2", try stringFromTurns(&.{ .@"L'", .B, .@"D'", .R2 }, &buffer));
    try testing.expectEqualStrings("R U R' U'", try stringFromTurns(try turnsFromString("R U R' U'", &turns), &buffer));
    try testing.expectEqualStrings("R2 L2 U2", try stringFromTurns(try turnsFromString("R2 L2 U2", &turns), &buffer));
    try testing.expectError(error.OutOfBounds, stringFromTurns(&.{ .U2, .U2, .U2, .U2 }, &buffer));
    try testing.expectError(error.OutOfBounds, stringFromTurns(&.{ .U2, .U2, .U2, .U, .U }, &buffer));
}

/// Same as `stringFromTurns`, but with the allocation done for you. The returned string has to be freed.
pub fn allocStringFromTurns(allocator: std.mem.Allocator, turns: []const Turn) ![]u8 {
    var len: usize = 0;
    for (turns) |turn| {
        len += turn_names[@intFromEnum(turn)].len;
    }
    len += @max(0, @as(i64, @intCast(turns.len)) - 1);

    const turn_string = try allocator.alloc(u8, len);
    const turn_string_copy = stringFromTurns(turns, turn_string) catch unreachable;
    assert(turn_string_copy.ptr == turn_string.ptr and turn_string_copy.len == turn_string.len);
    return turn_string;
}

test "allocStringFromTurns" {
    const expected = .{
        "",
        "R U R U",
        "L2 U L'",
        "B",
    };

    const actual = .{
        try allocStringFromTurns(testing.allocator, &.{}),
        try allocStringFromTurns(testing.allocator, &.{ .R, .U, .R, .U }),
        try allocStringFromTurns(testing.allocator, &.{ .L2, .U, .@"L'" }),
        try allocStringFromTurns(testing.allocator, &.{.B}),
    };
    defer inline for (actual) |turn_string| {
        testing.allocator.free(turn_string);
    };

    inline for (expected, actual) |expected_turn_string, actual_turn_string| {
        try testing.expectEqualStrings(expected_turn_string, actual_turn_string);
    }
}

// TODO helper functions and assertions everywhere, as well as more tests

///////////////////////////
// Turn table generation //
///////////////////////////

// The orientation of the last edge/corner can be determined from all other edges/corners. Therefore it does not need to
// be stored to fully represent a Rubik's cube, which is why 1 is subtracted away from edge/corner count. This reduces
// the size of the orientation turn tables at the expense of code complexity. The code to handle the missing edge/corner
// is pretty minimal and only affects turn table generation, so this is a trade-off definitely worth taking.
// Furthermore, this simplifies the process of generating a random Rubik's cube since there are no longer invalid
// orientations in the range 0..(edges/corners)_orientation_count.
pub const edges_orientation_count: comptime_int = std.math.pow(u16, edge_orientation_count, edge_count - 1);
pub const corners_orientation_count: comptime_int = std.math.pow(u16, corner_orientation_count, corner_count - 1);

pub const edges_permutation_count = factorial(edge_count);
pub const partial_edges_permutation_count = edges_permutation_count / factorial(edge_count - partial_edge_count);
pub const corners_permutation_count = factorial(corner_count);

pub var edges_orientation_turn_table: *const [edges_orientation_count * turn_count]u16 = undefined;
pub var corners_orientation_turn_table: *const [corners_orientation_count * turn_count]u16 = undefined;
/// This turn table is shared between the 3 sets of 4 edges.
pub var edges_permutation_turn_table: *const [partial_edges_permutation_count * turn_count]u16 = undefined;
pub var corners_permutation_turn_table: *const [corners_permutation_count * turn_count]u16 = undefined;

/// For comptime use. See `turn_table_infos`.
pub const TurnTableInfo = struct {
    name: []const u8,
    /// `size * ``zubik.turn_count` is the actual size of the turn table, not `size`.
    size: u16,
};

/// Information to operate different turn tables at comptime. Used by `applyTurnTable`.
pub const turn_table_infos = [total_parts]TurnTableInfo{
    .{ .name = "edges_orientation_turn_table", .size = edges_orientation_count },
    .{ .name = "corners_orientation_turn_table", .size = corners_orientation_count },
    .{ .name = "edges_permutation_turn_table", .size = partial_edges_permutation_count },
    .{ .name = "edges_permutation_turn_table", .size = partial_edges_permutation_count },
    .{ .name = "edges_permutation_turn_table", .size = partial_edges_permutation_count },
    .{ .name = "corners_permutation_turn_table", .size = corners_permutation_count },
};

/// Apply a turn to an individual part as opposed to applying it to the entire cube at once with `Cube.applyTurn`.
pub fn applyTurnTable(comptime index: comptime_int, val: u16, turn: Turn) u16 {
    return @field(@This(), turn_table_infos[index].name)[@as(usize, val) * turn_count + @intFromEnum(turn)];
}

// Orientation turn table generation. Orientation of edges is represented as a base 2 number, while orientation of
// corners is represented as a base 3 number, where the orientation of the nth piece is the nth digit. The following two
// functions simply get/set the nth digit of a number in arbitrary base.

fn getPieceOrientation(base: u8, orientation: u16, index: u8) u8 {
    return @intCast((orientation / std.math.pow(u16, base, index)) % base);
}

fn setPieceOrientation(base: u8, orientation: u16, index: u8, piece_orientation: u8) u16 {
    const prev_piece_orientation: i16 = getPieceOrientation(base, orientation, index);
    const diff = piece_orientation - prev_piece_orientation;
    return @intCast(@as(i16, @intCast(orientation)) + diff * std.math.pow(i16, base, index));
}

// Obtain the orientation of the last edge/corner, which is implicit. Store it as the most significant digit. This is
// necessary in the process of generating the orientation turn tables.
fn fullOrientation(base: u8, orientation: u16, max_piece_count: u8) u16 {
    var orientation_copy = orientation;
    var sum: u8 = 0;
    var i: u8 = 0;
    while (i < max_piece_count) : (i += 1) {
        sum += @intCast(orientation_copy % base);
        orientation_copy /= base;
    }
    const piece_orientation: u16 = @intCast(@mod(-@as(i8, @intCast(sum)), @as(i8, @intCast(base))));
    return orientation + piece_orientation * std.math.pow(u16, base, max_piece_count);
}

// Change the positions of the pieces without changing their orientation. In practice this just rotates the digits of
// orientation.
fn rotatePiecesOrientation(base: u8, orientation: u16, indices: anytype) u16 {
    var new_orientation = orientation;
    for (indices[0..(indices.len - 1)], 0..) |index, i| {
        const piece_orientation = getPieceOrientation(base, orientation, @intFromEnum(index));
        new_orientation = setPieceOrientation(base, new_orientation, @intFromEnum(indices[i + 1]), piece_orientation);
    }
    const piece_orientation = getPieceOrientation(base, orientation, @intFromEnum(indices[indices.len - 1]));
    return setPieceOrientation(base, new_orientation, @intFromEnum(indices[0]), piece_orientation);
}

// Change the orientation of the pieces and then change their positions using rotatePiecesOrientation().
fn offsetAndRotatePiecesOrientation(base: u8, orientation: u16, indices: anytype, offsets: anytype) u16 {
    var new_orientation = orientation;
    for (indices, offsets) |index, offset| {
        const piece_orientation = getPieceOrientation(base, new_orientation, @intFromEnum(index));
        const new_piece_orientation = (piece_orientation + @intFromEnum(offset)) % base;
        new_orientation = setPieceOrientation(base, new_orientation, @intFromEnum(index), new_piece_orientation);
    }
    return rotatePiecesOrientation(base, new_orientation, indices);
}

// Convert an array or a slice of EdgeOrientation/CornerOrientation elements into an orientation which can be used to
// index orientation turn tables. Used by Cube.init().
fn orientationFromArray(base: u8, orientation_array: anytype) u16 {
    var orientation: u16 = 0;
    var i: i8 = orientation_array.len - 2;
    while (i >= 0) : (i -= 1) {
        orientation = orientation * base + @intFromEnum(orientation_array[@intCast(i)]);
    }
    return orientation;
}

fn initEdgesOrientationTurnTable(allocator: std.mem.Allocator) !void {
    const table = try allocator.alloc(u16, edges_orientation_turn_table.len);
    var orientation: u16 = 0;
    while (orientation < edges_orientation_count) : (orientation += 1) {
        const full_orientation = fullOrientation(edge_orientation_count, orientation, edge_count - 1);
        var turn: u8 = 0;
        while (turn < turn_count) : (turn += 1) {
            const edges = face_edges_clockwise[turn / turns_per_face];
            const i = @as(usize, orientation) * turn_count + turn;
            table[i] = switch (@as(Turn, @enumFromInt(turn))) {
                .U, .D, .R, .L => rotatePiecesOrientation(
                    edge_orientation_count,
                    full_orientation,
                    edges,
                ),
                .@"U'", .@"D'", .@"R'", .@"L'" => rotatePiecesOrientation(
                    edge_orientation_count,
                    full_orientation,
                    reverse(&edges),
                ),
                .U2, .D2, .R2, .L2, .F2, .B2 => rotatePiecesOrientation(
                    edge_orientation_count,
                    rotatePiecesOrientation(
                        edge_orientation_count,
                        full_orientation,
                        [_]Edge{ edges[0], edges[2] },
                    ),
                    [_]Edge{ edges[1], edges[3] },
                ),
                .F, .B => offsetAndRotatePiecesOrientation(
                    edge_orientation_count,
                    full_orientation,
                    edges,
                    ([_]EdgeOrientation{.flipped} ** edges_per_face),
                ),
                .@"F'", .@"B'" => offsetAndRotatePiecesOrientation(
                    edge_orientation_count,
                    full_orientation,
                    reverse(&edges),
                    ([_]EdgeOrientation{.flipped} ** edges_per_face),
                ),
            } % edges_orientation_count;
        }
    }
    edges_orientation_turn_table = @ptrCast(table);
}

fn initCornersOrientationTurnTable(allocator: std.mem.Allocator) !void {
    const table = try allocator.alloc(u16, corners_orientation_turn_table.len);
    var orientation: u16 = 0;
    while (orientation < corners_orientation_count) : (orientation += 1) {
        const full_orientation = fullOrientation(corner_orientation_count, orientation, corner_count - 1);
        var turn: u8 = 0;
        while (turn < turn_count) : (turn += 1) {
            const corners = face_corners_clockwise[turn / turns_per_face];
            const i = @as(usize, orientation) * turn_count + turn;
            table[i] = switch (@as(Turn, @enumFromInt(turn))) {
                .U, .D => rotatePiecesOrientation(
                    corner_orientation_count,
                    full_orientation,
                    corners,
                ),
                .@"U'", .@"D'" => rotatePiecesOrientation(
                    corner_orientation_count,
                    full_orientation,
                    reverse(&corners),
                ),
                .U2, .D2, .R2, .L2, .F2, .B2 => rotatePiecesOrientation(
                    corner_orientation_count,
                    rotatePiecesOrientation(
                        corner_orientation_count,
                        full_orientation,
                        [_]Corner{ corners[0], corners[2] },
                    ),
                    [_]Corner{ corners[1], corners[3] },
                ),
                .R, .L, .F, .B => offsetAndRotatePiecesOrientation(
                    corner_orientation_count,
                    full_orientation,
                    corners,
                    ([_]CornerOrientation{ .clockwise, .counterclockwise } ** 2),
                ),
                .@"R'", .@"L'", .@"F'", .@"B'" => offsetAndRotatePiecesOrientation(
                    corner_orientation_count,
                    full_orientation,
                    reverse(&corners),
                    ([_]CornerOrientation{ .counterclockwise, .clockwise } ** 2),
                ),
            } % corners_orientation_count;
        }
    }
    corners_orientation_turn_table = @ptrCast(table);
}

// Permutation turn table generation. Permutation has two representations here: the standard array representation and an
// integer representation which is computed using Lehmer codes. The array representation is used to rotate pieces around
// to help with turn table generation, while the integer representation is used to index the generated turn tables. The
// following two functions simply switch back and forth between the two representations.
// https://gist.github.com/lukmdo/7049748

fn arrayFromPermutation(comptime permutation_len: u8, permutation: u16) [permutation_len]u8 {
    var permutation_array: [permutation_len]u8 = undefined;
    var i: u8 = 0;
    while (i < permutation_len) : (i += 1) {
        permutation_array[i] = i;
    }

    var permutation_remaining = permutation;
    i = 0;
    while (i < permutation_len) : (i += 1) {
        const j = permutation_remaining % (permutation_len - i);
        permutation_remaining /= permutation_len - i;
        std.mem.swap(u8, &permutation_array[i], &permutation_array[i + j]);
    }
    return permutation_array;
}

fn permutationFromArray(permutation_array: anytype, max_piece_count: u8) u16 {
    var base: [permutation_array.len]u8 = undefined;
    var positions: [permutation_array.len]u8 = undefined;
    var i: u8 = 0;
    while (i < permutation_array.len) : (i += 1) {
        base[i] = i;
        positions[i] = i;
    }

    var codes: [permutation_array.len]u8 = undefined;
    i = 0;
    while (i < permutation_array.len) : (i += 1) {
        const j = positions[permutation_array[i]];
        codes[i] = j - i;
        std.mem.swap(u8, &positions[base[i]], &positions[base[j]]);
        std.mem.swap(u8, &base[i], &base[j]);
    }

    var permutation: u16 = 0;
    i = @intCast(permutation_array.len - max_piece_count);
    while (i <= permutation_array.len) : (i += 1) {
        permutation = permutation * i + codes[permutation_array.len - i];
    }
    return permutation;
}

fn invert(permutation_array: anytype) [permutation_array.len]u8 {
    var permutation_array_inverse: [permutation_array.len]u8 = undefined;
    var i: u8 = 0;
    while (i < permutation_array.len) : (i += 1) {
        permutation_array_inverse[permutation_array[i]] = i;
    }
    return permutation_array_inverse;
}

// Same as rotatePiecesOrientation() but for the permutation array representation.
fn rotatePiecesPermutation(permutation_array: anytype, indices: anytype) [permutation_array.len]u8 {
    var new_permutation_array: [permutation_array.len]u8 = permutation_array;
    for (indices[0..(indices.len - 1)], 0..) |index, i| {
        new_permutation_array[@intFromEnum(indices[i + 1])] = permutation_array[@intFromEnum(index)];
    }
    new_permutation_array[@intFromEnum(indices[0])] = permutation_array[@intFromEnum(indices[indices.len - 1])];
    return new_permutation_array;
}

fn initEdgesPermutationTurnTable(allocator: std.mem.Allocator) !void {
    const table = try allocator.alloc(u16, edges_permutation_turn_table.len);
    var permutation: u16 = 0;
    while (permutation < partial_edges_permutation_count) : (permutation += 1) {
        // There are two calls to invert() in initEdgesPermutationTurnTable(), but none in
        // initCornersPermutationTurnTable(). This is due to the fact that only 4 out of 12 edges are used and
        // permutationFromArray() only works with 4 out of 12 entries if these invert() calls are present.
        const permutation_array = invert(arrayFromPermutation(edge_count, permutation));
        var turn: u8 = 0;
        while (turn < turn_count) : (turn += 1) {
            const edges = face_edges_clockwise[turn / turns_per_face];
            const i = @as(usize, permutation) * turn_count + turn;
            table[i] = permutationFromArray(invert(switch (@as(Turn, @enumFromInt(turn))) {
                .U, .D, .R, .L, .F, .B => rotatePiecesPermutation(
                    permutation_array,
                    edges,
                ),
                .@"U'", .@"D'", .@"R'", .@"L'", .@"F'", .@"B'" => rotatePiecesPermutation(
                    permutation_array,
                    reverse(&edges),
                ),
                .U2, .D2, .R2, .L2, .F2, .B2 => rotatePiecesPermutation(
                    rotatePiecesPermutation(permutation_array, [_]Edge{ edges[0], edges[2] }),
                    [_]Edge{ edges[1], edges[3] },
                ),
            }), partial_edge_count - 1);
        }
    }
    edges_permutation_turn_table = @ptrCast(table);
}

fn initCornersPermutationTurnTable(allocator: std.mem.Allocator) !void {
    const table = try allocator.alloc(u16, corners_permutation_turn_table.len);
    var permutation: u16 = 0;
    while (permutation < corners_permutation_count) : (permutation += 1) {
        const permutation_array = arrayFromPermutation(corner_count, permutation);
        var turn: u8 = 0;
        while (turn < turn_count) : (turn += 1) {
            const corners = face_corners_clockwise[turn / turns_per_face];
            const i = @as(usize, permutation) * turn_count + turn;
            table[i] = permutationFromArray(switch (@as(Turn, @enumFromInt(turn))) {
                .U, .D, .R, .L, .F, .B => rotatePiecesPermutation(
                    permutation_array,
                    corners,
                ),
                .@"U'", .@"D'", .@"R'", .@"L'", .@"F'", .@"B'" => rotatePiecesPermutation(
                    permutation_array,
                    reverse(&corners),
                ),
                .U2, .D2, .R2, .L2, .F2, .B2 => rotatePiecesPermutation(
                    rotatePiecesPermutation(permutation_array, [_]Corner{ corners[0], corners[2] }),
                    [_]Corner{ corners[1], corners[3] },
                ),
            }, corner_count - 1);
        }
    }
    corners_permutation_turn_table = @ptrCast(table);
}

fn initAllTurnTables(allocator: std.mem.Allocator) !void {
    try initEdgesOrientationTurnTable(allocator);
    try initCornersOrientationTurnTable(allocator);
    try initEdgesPermutationTurnTable(allocator);
    try initCornersPermutationTurnTable(allocator);
}

/// An arena allocator is recommended. Caching to a file is used to speed up the initialization. Set `cache_sub_path` to
/// `null` to disable caching. `cache_sub_path` is relative to the executable directory and not the current working
/// directory. Takes ~35ms to complete without caching and less than 1ms with caching. See also `initPartialTables` and
/// `initInstantTables`.
pub fn initTurnTables(allocator: std.mem.Allocator, cache_sub_path: ?[]const u8) !void {
    if (cache_sub_path) |sub_path| {
        var exe_path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const exe_path = try std.fs.selfExeDirPath(&exe_path_buf);
        var exe_dir = try std.fs.openDirAbsolute(exe_path, .{});
        defer exe_dir.close();

        if (exe_dir.openFile(sub_path, .{})) |file| {
            defer file.close();
            const edges_orientation_turn_table_bytes =
                try allocator.alignedAlloc(u8, 2, edges_orientation_turn_table.len * 2);
            const corners_orientation_turn_table_bytes =
                try allocator.alignedAlloc(u8, 2, corners_orientation_turn_table.len * 2);
            const edges_permutation_turn_table_bytes =
                try allocator.alignedAlloc(u8, 2, edges_permutation_turn_table.len * 2);
            const corners_permutation_turn_table_bytes =
                try allocator.alignedAlloc(u8, 2, corners_permutation_turn_table.len * 2);

            _ = try file.readAll(edges_orientation_turn_table_bytes);
            _ = try file.readAll(corners_orientation_turn_table_bytes);
            _ = try file.readAll(edges_permutation_turn_table_bytes);
            _ = try file.readAll(corners_permutation_turn_table_bytes);

            edges_orientation_turn_table = @ptrCast(edges_orientation_turn_table_bytes);
            corners_orientation_turn_table = @ptrCast(corners_orientation_turn_table_bytes);
            edges_permutation_turn_table = @ptrCast(edges_permutation_turn_table_bytes);
            corners_permutation_turn_table = @ptrCast(corners_permutation_turn_table_bytes);
        } else |_| {
            try initAllTurnTables(allocator);
            const file = try exe_dir.createFile(sub_path, .{});
            defer file.close();
            try file.writeAll(@as(
                *const [edges_orientation_turn_table.len * 2]u8,
                @ptrCast(edges_orientation_turn_table),
            ));
            try file.writeAll(@as(
                *const [corners_orientation_turn_table.len * 2]u8,
                @ptrCast(corners_orientation_turn_table),
            ));
            try file.writeAll(@as(
                *const [edges_permutation_turn_table.len * 2]u8,
                @ptrCast(edges_permutation_turn_table),
            ));
            try file.writeAll(@as(
                *const [corners_permutation_turn_table.len * 2]u8,
                @ptrCast(corners_permutation_turn_table),
            ));
        }
    } else {
        try initAllTurnTables(allocator);
    }
}

pub fn deinitTurnTables(allocator: std.mem.Allocator) void {
    allocator.free(edges_orientation_turn_table);
    allocator.free(corners_orientation_turn_table);
    allocator.free(edges_permutation_turn_table);
    allocator.free(corners_permutation_turn_table);
}

//
// TODO
//

pub const Cube = struct {
    edges_orientation: u16,
    corners_orientation: u16,
    edges_permutation: [edges_parts]u16,
    corners_permutation: u16,

    // TODO isValid

    pub fn init(
        edges_orientation: *const [edge_count]EdgeOrientation,
        corners_orientation: *const [corner_count]CornerOrientation,
        edges_permutation: *const [edge_count]Edge,
        corners_permutation: *const [corner_count]Corner,
    ) Cube {
        return .{
            .edges_orientation = orientationFromArray(edge_orientation_count, edges_orientation),
            .corners_orientation = orientationFromArray(corner_orientation_count, corners_orientation),
            .edges_permutation = blk: {
                var permutation: [edges_parts]u16 = undefined;
                var i: u8 = 0;
                while (i < edges_parts) : (i += 1) {
                    var partial_edges_permutation: [edge_count]u8 =
                        invert(@as(*const [edge_count]u8, @ptrCast(edges_permutation)));
                    for (0..partial_edge_count) |j| {
                        const k = j + partial_edge_count * i;
                        std.mem.swap(u8, &partial_edges_permutation[j], &partial_edges_permutation[k]);
                    }
                    permutation[i] = permutationFromArray(partial_edges_permutation, partial_edge_count - 1);
                }
                break :blk permutation;
            },
            .corners_permutation = permutationFromArray(
                @as(*const [corner_count]u8, @ptrCast(corners_permutation)),
                corner_count - 1,
            ),
        };
    }

    /// Access a field by index instead of by name. See also `setPart`.
    pub fn getPart(self: Cube, comptime index: comptime_int) u16 {
        return switch (index) {
            0 => self.edges_orientation,
            1 => self.corners_orientation,
            2...4 => self.edges_permutation[index - 2],
            5 => self.corners_permutation,
            else => unreachable,
        };
    }

    /// Access and modify a field by index instead of by name. See also `getPart`.
    pub fn setPart(self: *Cube, comptime index: comptime_int, val: u16) void {
        switch (index) {
            0 => self.edges_orientation = val,
            1 => self.corners_orientation = val,
            2...4 => self.edges_permutation[index - 2] = val,
            5 => self.corners_permutation = val,
            else => unreachable,
        }
    }

    pub fn applyTurn(self: Cube, turn: Turn) Cube {
        var new_self = self;
        inline for (0..total_parts) |i| {
            new_self.setPart(i, applyTurnTable(i, self.getPart(i), turn));
        }
        return new_self;
    }

    pub fn applyTurns(self: Cube, turns: []const Turn) Cube {
        var new_self = self;
        for (turns) |turn| {
            new_self = new_self.applyTurn(turn);
        }
        return new_self;
    }

    /// Same as `Cube.applyTurns(turnsFromString(turn_string))`, but without using any additional memory.
    pub fn applyTurnString(self: Cube, turn_string: []const u8) !Cube {
        var new_self = self;
        var it = std.mem.splitScalar(u8, turn_string, ' ');
        while (it.next()) |turn_name| {
            if (turn_map.get(turn_name)) |turn| {
                new_self = new_self.applyTurn(turn);
            } else {
                return error.InvalidTurnString;
            }
        }
        return new_self;
    }
};

pub const solved_cube = Cube.init(
    &(.{.correct} ** edge_count),
    &(.{.correct} ** corner_count),
    @ptrCast(std.enums.values(Edge)),
    @ptrCast(std.enums.values(Corner)),
);

pub const superflip_cube = Cube.init(
    &(.{.flipped} ** edge_count),
    &(.{.correct} ** corner_count),
    @ptrCast(std.enums.values(Edge)),
    @ptrCast(std.enums.values(Corner)),
);

// test "Cube.applyTurnString" {
//     var cube1 = solved_cube;
//     var cube2 = solved_cube;
//     _ = cube2;
//     // TODO string to turn
//     cube1.applyTurns();
// }

test "Cube turning" {
    try initTurnTables(testing.allocator, null);
    defer deinitTurnTables(testing.allocator);

    try testing.expectEqual(
        superflip_cube,
        try solved_cube.applyTurnString("U R2 F B R B2 R U2 L B2 R U' D' R2 F R' L B2 U2 F2"),
    );

    try testing.expectEqual(
        Cube.init(
            &.{
                .correct, .flipped, .flipped, .flipped,
                .flipped, .correct, .correct, .correct,
                .correct, .flipped, .correct, .flipped,
            },
            &.{ .correct, .clockwise, .clockwise, .correct, .clockwise, .counterclockwise, .correct, .clockwise },
            &.{ .LB, .UB, .UL, .DB, .UF, .DL, .RB, .RF, .LF, .UR, .DF, .DR },
            &.{ .DRF, .URF, .DLF, .ULB, .DRB, .ULF, .DLB, .URB },
        ),
        try solved_cube.applyTurnString("L2 R2 U B2 D U R2 D B2 D2 R2 F' R2 U L2 F' L D R B F"),
    );
}

test "Turn table caching" {
    try initTurnTables(testing.allocator, null);
    const edges_orientation_turn_table_copy = try testing.allocator.alloc(u16, edges_orientation_turn_table.len);
    const corners_orientation_turn_table_copy = try testing.allocator.alloc(u16, corners_orientation_turn_table.len);
    const edges_permutation_turn_table_copy = try testing.allocator.alloc(u16, edges_permutation_turn_table.len);
    const corners_permutation_turn_table_copy = try testing.allocator.alloc(u16, corners_permutation_turn_table.len);

    defer testing.allocator.free(edges_orientation_turn_table_copy);
    defer testing.allocator.free(corners_orientation_turn_table_copy);
    defer testing.allocator.free(edges_permutation_turn_table_copy);
    defer testing.allocator.free(corners_permutation_turn_table_copy);

    @memcpy(edges_orientation_turn_table_copy, edges_orientation_turn_table);
    @memcpy(corners_orientation_turn_table_copy, corners_orientation_turn_table);
    @memcpy(edges_permutation_turn_table_copy, edges_permutation_turn_table);
    @memcpy(corners_permutation_turn_table_copy, corners_permutation_turn_table);

    deinitTurnTables(testing.allocator);
    // The first initTurnTables() call creates the cache, the second initTurnTables() call loads from the created cache.
    try initTurnTables(testing.allocator, "turn_tables.cache");
    deinitTurnTables(testing.allocator);
    try initTurnTables(testing.allocator, "turn_tables.cache");
    defer deinitTurnTables(testing.allocator);

    try testing.expectEqualSlices(u16, edges_orientation_turn_table_copy, edges_orientation_turn_table);
    try testing.expectEqualSlices(u16, corners_orientation_turn_table_copy, corners_orientation_turn_table);
    try testing.expectEqualSlices(u16, edges_permutation_turn_table_copy, edges_permutation_turn_table);
    try testing.expectEqualSlices(u16, corners_permutation_turn_table_copy, corners_permutation_turn_table);
}

/////////////
// Solving //
/////////////

pub const PartialTable = struct {
    table: [*]const u8,
    max_depth: u8,
};
pub var partial_tables: [partial_table_count]PartialTable = undefined;

pub fn indexOfPartialTable(comptime indices: [2]comptime_int) comptime_int {
    return partial_table_count - (total_parts - indices[0]) * (total_parts - indices[0] - 1) / 2 +
        (indices[1] - indices[0] - 1);
}

fn initPartialTable(comptime indices: [2]comptime_int, allocator: std.mem.Allocator) !PartialTable {
    const offset_multiplier = turn_table_infos[indices[1]].size;
    const table =
        try allocator.alloc(u8, @as(usize, turn_table_infos[indices[0]].size) * offset_multiplier);
    @memset(table, 0);

    var turn: u8 = 0;
    while (turn < turn_count) : (turn += 1) {
        table[
            @as(usize, applyTurnTable(indices[0], solved_cube.getPart(indices[0]), @enumFromInt(turn))) *
                offset_multiplier + applyTurnTable(indices[1], solved_cube.getPart(indices[1]), @enumFromInt(turn))
        ] = 1;
    }

    var curr_depth: u8 = 1;
    var done = false;
    while (!done) : (curr_depth += 1) {
        done = true;
        for (table, 0..) |depth, i| {
            if (depth == curr_depth) {
                const parts = [_]u16{ @intCast(i / offset_multiplier), @intCast(i % offset_multiplier) };
                turn = 0;
                while (turn < turn_count) : (turn += 1) {
                    const j = @as(usize, applyTurnTable(indices[0], parts[0], @enumFromInt(turn))) * offset_multiplier +
                        applyTurnTable(indices[1], parts[1], @enumFromInt(turn));
                    if (table[j] == 0) {
                        table[j] = curr_depth + 1;
                        done = false;
                    }
                }
            }
        }
    }

    table[@as(usize, solved_cube.getPart(indices[0])) * offset_multiplier + solved_cube.getPart(indices[1])] = 0;
    return .{ .table = @ptrCast(table), .max_depth = curr_depth - 1 };
}

fn initAllPartialTables(allocator: std.mem.Allocator) !void {
    inline for (0..total_parts) |i| {
        inline for ((i + 1)..total_parts) |j| {
            partial_tables[indexOfPartialTable(.{ i, j })] = try initPartialTable(.{ i, j }, allocator);
        }
    }
}

/// An arena allocator is recommended. Caching to a file is used to speed up the initialization. Set `cache_sub_path` to
/// `null` to disable caching. `cache_sub_path` is relative to the executable directory and not the current working
/// directory. Takes ~70s to complete without caching and less than 1s with caching. See also `initTurnTables` and
/// `initInstantTables`.
pub fn initPartialTables(allocator: std.mem.Allocator, cache_sub_path: ?[]const u8) !void {
    if (cache_sub_path) |sub_path| {
        var exe_path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const exe_path = try std.fs.selfExeDirPath(&exe_path_buf);
        var exe_dir = try std.fs.openDirAbsolute(exe_path, .{});
        defer exe_dir.close();

        if (exe_dir.openFile(sub_path, .{})) |file| {
            defer file.close();
            inline for (0..total_parts) |i| {
                inline for ((i + 1)..total_parts) |j| {
                    const table = try allocator.alloc(
                        u8,
                        @as(usize, turn_table_infos[i].size) * turn_table_infos[j].size,
                    );
                    _ = try file.readAll(table);
                    const k = indexOfPartialTable(.{ i, j });
                    partial_tables[k].table = @ptrCast(table);
                    _ = try file.readAll(@as([*]u8, @ptrCast(&partial_tables[k].max_depth))[0..1]);
                }
            }
        } else |_| {
            try initAllPartialTables(allocator);
            const file = try exe_dir.createFile(sub_path, .{});
            defer file.close();
            inline for (0..total_parts) |i| {
                inline for ((i + 1)..total_parts) |j| {
                    const k = indexOfPartialTable(.{ i, j });
                    try file.writeAll(partial_tables[k].table[0..(@as(usize, turn_table_infos[i].size) *
                        turn_table_infos[j].size)]);
                    try file.writeAll(@as([*]u8, @ptrCast(&partial_tables[k].max_depth))[0..1]);
                }
            }
        }
    } else {
        try initAllPartialTables(allocator);
    }
}

pub fn deinitPartialTables(allocator: std.mem.Allocator) void {
    inline for (0..total_parts) |i| {
        inline for ((i + 1)..total_parts) |j| {
            const len = @as(usize, turn_table_infos[i].size) * turn_table_infos[j].size;
            allocator.free(partial_tables[indexOfPartialTable(.{ i, j })].table[0..len]);
        }
    }
}

// TODO comprehensive when async is reimplemented into zig
fn solveDepthImpl(
    comptime indices: []const comptime_int,
    turn_set: []const Turn,
    parts: [indices.len]u16,
    curr_depth: u8,
    max_depth: u8,
    solution: [*]Turn,
) bool {
    if (curr_depth == max_depth) {
        var solved = true;
        inline for (indices, 0..) |index, i| {
            solved = solved and parts[i] == solved_cube.getPart(index);
        }
        return solved;
    } else {
        var turn_index: u8 = 0;
        outer: while (turn_index < turn_set.len) : (turn_index += 1) {
            const turn = if (turn_set.len == turn_count) turn_index else @intFromEnum(turn_set[turn_index]);
            if (curr_depth > 0) {
                if (turn / turns_per_face == @intFromEnum(solution[curr_depth - 1]) / turns_per_face) {
                    continue;
                }

                const prev_turn_axis = @intFromEnum(solution[curr_depth - 1]) / turns_per_axis;
                if (curr_depth > 1 and turn / turns_per_axis == prev_turn_axis and
                    prev_turn_axis == @intFromEnum(solution[curr_depth - 2]) / turns_per_axis)
                {
                    continue;
                }
            }

            var new_parts: [indices.len]u16 = undefined;
            inline for (indices, 0..) |index, i| {
                new_parts[i] = applyTurnTable(index, parts[i], @enumFromInt(turn));
            }

            inline for (0..indices.len) |i| {
                inline for ((i + 1)..indices.len) |j| {
                    const depth_left = max_depth - curr_depth - 1;
                    const k = indexOfPartialTable(.{ indices[i], indices[j] });
                    if (depth_left < partial_tables[k].max_depth and partial_tables[k].table[@as(usize, new_parts[i]) * turn_table_infos[indices[j]].size + new_parts[j]] > depth_left) {
                        continue :outer;
                    }
                }
            }

            solution[curr_depth] = @enumFromInt(turn);
            if (solveDepthImpl(indices, turn_set, new_parts, curr_depth + 1, max_depth, solution)) {
                return true;
            }
        }
        return false;
    }
}

fn solveImpl(comptime indices: []const comptime_int, turn_set: []const Turn, parts: [indices.len]u16, solution: [*]Turn) u8 {
    var depth: u8 = 0;
    var solved = false;
    while (!solved) : (depth += 1) {
        solved = solveDepthImpl(indices, turn_set, parts, 0, depth, solution);
    }
    return depth - 1;
}

pub fn solveDepth(cube: Cube, depth: u8, solution: [*]Turn) bool {
    var parts: [total_parts]u16 = undefined;
    inline for (0..total_parts) |i| {
        parts[i] = cube.getPart(i);
    }
    return solveDepthImpl(&.{ 0, 1, 2, 3, 4, 5 }, std.enums.values(Turn), parts, 0, depth, solution);
}

pub fn solve(cube: Cube, solution: [*]Turn) u8 {
    var depth: u8 = 0;
    var solved = false;
    while (!solved) : (depth += 1) {
        solved = solveDepth(cube, depth, solution);
    }
    return depth - 1;
}

pub fn solveQuick(cube: Cube, solution: [*]Turn) u8 {
    var parts: [total_parts]u16 = undefined;
    inline for (0..total_parts) |i| {
        parts[i] = cube.getPart(i);
    }

    var depth1: u8 = 0;
    var solved = false;
    while (!solved) : (depth1 += 1) {
        solved = solveDepthImpl(&.{ 0, 1, 4 }, std.enums.values(Turn), .{ parts[0], parts[1], parts[4] }, 0, depth1, solution);
    }
    depth1 -= 1;

    for (solution[0..depth1]) |turn| {
        inline for (0..total_parts) |i| {
            parts[i] = applyTurnTable(i, parts[i], turn);
        }
    }

    var depth2: u8 = 0;
    solved = false;
    while (!solved) : (depth2 += 1) {
        solved = solveDepthImpl(&.{ 0, 1, 2, 3, 4, 5 }, &.{ .U, .U2, .@"U'", .D, .D2, .@"D'", .R2, .L2, .F2, .B2 }, parts, 0, depth2, @ptrCast(&solution[depth1]));
    }
    return depth1 + depth2 - 1;
}

pub var instant_tables: [total_parts][*]const []const Turn = undefined;

fn initInstantTable(comptime index: comptime_int, allocator: std.mem.Allocator) !void {
    const table = try allocator.alloc([]Turn, turn_table_infos[index].size);
    var solution: [gods_number]Turn = undefined;
    var parts: [index + 1]u16 = undefined;
    comptime var indices: [index + 1]comptime_int = undefined;
    inline for (0..index) |i| {
        parts[i] = solved_cube.getPart(i);
        indices[i] = i;
    }
    parts[index] = 0;
    indices[index] = index;

    outer: while (parts[index] < table.len) : (parts[index] += 1) {
        std.debug.print("{d}\n", .{parts[index]});
        // Not all edge orientations are valid which means solveImpl() will never finish. Therefore, when index is 3 or
        // 4, the placement of the edges needs to be checked to ensure that no 2 edges overlap.
        if (index == 3 or index == 4) {
            const permutation_array = arrayFromPermutation(edge_count, parts[index]);
            var i: u8 = 0;
            while (i < partial_edge_count) : (i += 1) {
                if (permutation_array[i] < (index - 2) * partial_edge_count) {
                    table[parts[index]] = try allocator.alloc(Turn, 0);
                    continue :outer;
                }
            }
        }

        // Similarly, not all corner permutations are solvable when all the edges are solved. Parity therefore has to be
        // checked when index is 5.
        if (index == 5 and parity(arrayFromPermutation(corner_count, parts[index]))) {
            table[parts[index]] = try allocator.alloc(Turn, 0);
            continue;
        }

        // TODO run without shortcuts
        var depth: u8 = undefined;
        if (index == 5) {
            depth = solveQuick(Cube{
                .edges_orientation = parts[0],
                .corners_orientation = parts[1],
                .edges_permutation = .{ parts[2], parts[3], parts[4] },
                .corners_permutation = parts[5],
            }, &solution);
        } else {
            depth = solveImpl(&indices, std.enums.values(Turn), parts, &solution);
        }
        table[parts[index]] = try allocator.alloc(Turn, depth);
        @memcpy(table[parts[index]], solution[0..depth]);
    }
    instant_tables[index] = @ptrCast(table);
}

fn initAllInstantTables(allocator: std.mem.Allocator) !void {
    inline for (0..total_parts) |i| {
        try initInstantTable(i, allocator);
    }
}

// TODO times
/// An arena allocator is highly recommended. Caching to a file is used to speed up the initialization. Set
/// `cache_sub_path` to `null` to disable caching. `cache_sub_path` is relative to the executable directory and not the
/// current working directory. Takes ~TODOs to complete without caching and less than TODOs with caching. See also `initTurnTables` and `initPartialTables`.
pub fn initInstantTables(allocator: std.mem.Allocator, cache_sub_path: ?[]const u8) !void {
    if (cache_sub_path) |sub_path| {
        var exe_path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
        const exe_path = try std.fs.selfExeDirPath(&exe_path_buf);
        var exe_dir = try std.fs.openDirAbsolute(exe_path, .{});
        defer exe_dir.close();

        if (exe_dir.openFile(sub_path, .{})) |file| {
            defer file.close();
            var i: u8 = 0;
            while (i < total_parts) : (i += 1) {
                const table = try allocator.alloc([]Turn, turn_table_infos[i].size);
                var j: u16 = 0;
                while (j < turn_table_infos[i].size) : (j += 1) {
                    var solution: []Turn = undefined;
                    _ = try file.readAll(@as([*]u8, @ptrCast(&solution.len))[0..@sizeOf(usize)]);
                    solution = try allocator.alloc(Turn, solution.len);
                    _ = try file.readAll(@ptrCast(solution));
                    table[j] = solution;
                }
                instant_tables[i] = @ptrCast(table);
            }
        } else |_| {
            try initAllInstantTables(allocator);
            const file = try exe_dir.createFile(sub_path, .{});
            defer file.close();
            var i: u8 = 0;
            while (i < total_parts) : (i += 1) {
                var j: u16 = 0;
                while (j < turn_table_infos[i].size) : (j += 1) {
                    try file.writeAll(@as([*]const u8, @ptrCast(&instant_tables[i][j].len))[0..@sizeOf(usize)]);
                    try file.writeAll(@ptrCast(instant_tables[i][j]));
                }
            }
        }
    } else {
        try initAllInstantTables(allocator);
    }
}

pub fn deinitInstantTables(allocator: std.mem.Allocator) void {
    var i: u8 = 0;
    while (i < total_parts) : (i += 1) {
        defer allocator.free(instant_tables[i][0..turn_table_infos[i].size]);
        var j: u16 = 0;
        while (j < turn_table_infos[i].size) : (j += 1) {
            allocator.free(instant_tables[i][j]);
        }
    }
}

pub fn solveInstant(cube: Cube) [total_parts][]const Turn {
    var cube_copy = cube;
    var solution: [total_parts][]const Turn = undefined;
    inline for (0..total_parts) |i| {
        solution[i] = instant_tables[i][cube_copy.getPart(i)];
        cube_copy = cube_copy.applyTurns(solution[i]);
    }
    return solution;
}
