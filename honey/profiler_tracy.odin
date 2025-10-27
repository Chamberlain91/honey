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

_profiler_set_thread_name :: proc(name: cstring) {

    when ENABLE_TRACY_PROFILER {

        tracy.SetThreadName(name)

    }
}

_profiler_mark_frame :: proc(name: cstring = nil) {

    when ENABLE_TRACY_PROFILER {

        if !_capture do return
        tracy.FrameMark(name)

    }
}

when ENABLE_TRACY_PROFILER {

    _profiler_zone :: tracy.ZoneN

} else {

    _profiler_zone :: proc(name: string) {
        // Nothing
    }

}

enable_profile_capture :: proc(enable: bool) {
    _capture = enable
}

@(deferred_in = profile_scope_end)
profile_scoped_event :: #force_inline proc "contextless" (name: string, loc := #caller_location) -> bool {
    if !_capture do return true
    profile_scope_begin(name, loc)
    return true
}

profile_scope_begin :: #force_inline proc "contextless" (name: string, loc := #caller_location) {
    if !_capture do return
    context = runtime.default_context()
    append(&_zones, tracy.ZoneBegin(true, tracy.TRACY_CALLSTACK, loc))
}

profile_scope_end :: #force_inline proc "contextless" (_: string = "", loc := #caller_location) {
    if !_capture do return
    context = runtime.default_context()
    tracy.ZoneEnd(pop(&_zones, loc))
}

@(thread_local, private = "file")
_zones: [dynamic]tracy.ZoneCtx

@(private = "file")
_capture: bool
