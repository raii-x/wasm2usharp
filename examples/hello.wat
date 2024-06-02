(module
  (import "hello_env" "debug_log" (func $debug_log (param i32 i32)))
  ;; Prints the range from 0 to $str_end in the memory as a string.
  (func (export "hello")
    i32.const 0
    global.get $str_end
    call $debug_log
  )
  (memory (export "memory") 1)
  (global $str_end (export "str_end") (mut i32) i32.const 7)
  (data (i32.const 0) "Hello, ")
)
