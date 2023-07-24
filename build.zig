const std = @import("std");

pub fn build(builder: *std.Build) void {
    const target = builder.standardTargetOptions(.{});
    const optimize = builder.standardOptimizeOption(.{});

    const exe = builder.addExecutable(.{
        .name = "Zubik",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    builder.installArtifact(exe);

    const run_cmd = builder.addRunArtifact(exe);
    run_cmd.step.dependOn(builder.getInstallStep());
    if (builder.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = builder.step("run", "Run Zubik");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = builder.addTest(.{
        .root_source_file = .{ .path = "src/zubik.zig" },
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = builder.addRunArtifact(unit_tests);
    const test_step = builder.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const docs = builder.addTest(.{ .root_source_file = .{ .path = "src/zubik.zig" } });
    docs.emit_docs = .emit;
    const docs_step = builder.step("docs", "Generate documentation");
    docs_step.dependOn(&docs.step);
}
