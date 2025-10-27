#+no-instrumentation
package honey

import "base:runtime"
import "core:mem"

ENABLE_TRACY_PROFILER: bool : #config(TRACY_ENABLE, false)

@(require) import tracy "shared:odin-tracy"

_initialize_profiler :: proc(allocator: mem.Allocator) -> mem.Allocator {

    when ENABLE_TRACY_PROFILER {

        @(static) self: tracy.ProfiledAllocatorData

        // Profile heap allocations with Tracy for this context.
        return tracy.MakeProfiledAllocator(
            self = &self,
            callstack_size = 5,
            backing_allocator = allocator,
            secure = true,
        )

    } else {

        // No profiler enabled, pass through.
        return allocator

    }
}

@(disabled = !ENABLE_TRACY_PROFILER)
_profiler_set_thread_name :: proc(name: cstring) {
    tracy.SetThreadName(name)
}

@(disabled = !ENABLE_TRACY_PROFILER)
_profiler_mark_frame :: proc(name: cstring = nil) {
    if !_capture do return
    tracy.FrameMark(name)
}

_profiler_zone :: tracy.ZoneN

enable_profile_capture :: proc(enable: bool) {
    _capture = enable
}

@(deferred_in = profile_scope_end)
profile_scoped_event :: #force_inline proc "contextless" (name: string, loc := #caller_location) -> bool {
    when ENABLE_TRACY_PROFILER {
        if !_capture do return true
        profile_scope_begin(name, loc)
    }
    return true
}

@(disabled = !ENABLE_TRACY_PROFILER)
profile_scope_begin :: #force_inline proc "contextless" (name: string, loc := #caller_location) {
    if !_capture do return
    context = runtime.default_context()
    zctx := tracy.ZoneBegin(true, tracy.TRACY_CALLSTACK, loc)
    tracy.ZoneName(zctx, name)
    append(&_zones, zctx)
}

@(disabled = !ENABLE_TRACY_PROFILER)
profile_scope_end :: #force_inline proc "contextless" (_: string = "", loc := #caller_location) {
    if !_capture do return
    context = runtime.default_context()
    tracy.ZoneEnd(pop(&_zones, loc))
}

@(thread_local, private = "file")
_zones: [dynamic]tracy.ZoneCtx

@(private = "file")
_capture: bool
