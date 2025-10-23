#+private file
#+no-instrumentation
package honey

@(require) import "base:runtime"
@(require) import "core:fmt"
@(require) import "core:prof/spall"
@(require) import "core:sync"

ENALBE_SPALL_PROFILER :: #config(PROFILE, false)

@(disabled = !ENALBE_SPALL_PROFILER, private = "package")
PROFILE_SCOPE_BEGIN :: proc(name: string, loc := #caller_location) {
    if !spall_enable do return
    spall._buffer_begin(&spall_context, spall_get_buffer(), name, location = loc)
}

@(disabled = !ENALBE_SPALL_PROFILER, private = "package")
PROFILE_SCOPE_END :: proc() {
    if !spall_enable do return
    spall._buffer_end(&spall_context, spall_get_buffer())
}

@(deferred_none = PROFILE_SCOPE_END, private = "package")
PROFILE_SCOPED_EVENT :: proc(name: string, loc := #caller_location) -> bool {
    if !spall_enable do return true
    when ENALBE_SPALL_PROFILER {
        PROFILE_SCOPE_BEGIN(name, loc)
    }
    return true
}

@(private = "package", disabled = !ENALBE_SPALL_PROFILER)
enable_profiling :: proc(enable: bool) {
    spall_enable = enable
}

spall_context: spall.Context
spall_buffers: [dynamic]^spall.Buffer
spall_enable: bool

when ENALBE_SPALL_PROFILER {

    @(init, no_instrumentation)
    _spall_static_init :: proc "contextless" () {
        context = runtime.default_context()
        spall_context = spall.context_create("profile.spall")
        fmt.printfln("[INFO] Initialized spall.")
    }

    @(fini, no_instrumentation)
    _spall_static_exit :: proc "contextless" () {
        context = runtime.default_context()

        fmt.printfln("[INFO] Finalizing {} spall buffers.", len(spall_buffers))
        for buffer in spall_buffers {
            spall.buffer_destroy(&spall_context, buffer)
        }

        fmt.println("[INFO] Finalizing spall context.")
        spall.context_destroy(&spall_context)
    }
}

@(no_instrumentation)
spall_get_buffer :: proc() -> ^spall.Buffer {

    when ENALBE_SPALL_PROFILER {

        @(thread_local)
        spall_buffer: spall.Buffer

        @(thread_local)
        memory: [spall.BUFFER_DEFAULT_SIZE]u8

        if spall_buffer.data == nil {
            fmt.printfln("[INFO] Constructing spall buffer for thread {}.", sync.current_thread_id())
            spall_buffer = spall.buffer_create(memory[:], u32(sync.current_thread_id()))
            append(&spall_buffers, &spall_buffer)
        }

        return &spall_buffer

    }

    return nil
}
