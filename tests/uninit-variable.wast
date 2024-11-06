(module
  (func (export "copy-propagation-param") (param i32) (result i32)
    loop
      i32.const 1
      local.tee 0
      i32.eqz
      br_if 0
      local.get 0
      i32.const 0
      i32.add
      local.tee 0
      i32.eqz
      br_if 0
    end
    local.get 0
  )
)

(assert_return (invoke "copy-propagation-param" (i32.const 0)) (i32.const 1))
