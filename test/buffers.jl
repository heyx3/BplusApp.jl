@std140 struct S140
    f::Float32
    i::Int64
    a::StaticBlockArray{128, Bool}
end
@std430 struct S430
    f::Float32
    i::Int64
    a::StaticBlockArray{128, Bool}
end

Random.seed!(0x33445566)
bp_gl_context(v2i(800, 500), "Test: Buffers") do context::Context
    for T in [ Float32, StaticBlockArray{2, v2f, OglBlock_std140}, StaticBlockArray{2, v2f, OglBlock_std430},
               S140, StaticBlockArray{4, S140},
               S430, StaticBlockArray{4, S430} ]
        test_t = rand(T)
        buf_t = Buffer(true, test_t)
        test_t2::T = get_buffer_data(buf_t, T)
        @bp_check(test_t == test_t2,
                  "Expected:\n", test_t,
                  "\n\nGot:\n", test_t2)

        !(T <: StaticBlockArray) &&
         for (src_size, first_src_element, (dest_size, dest_expected_size)) in [ (8, 1, (8, 8)),
                                                                                 (2, 2, (1, 1)),
                                                                                 (2, 2, (nothing, 1)),
                                                                                 (8, 1, (nothing, 8)),
                                                                                 (4, 3, (nothing, 2)),
                                                                                 (8, 3, (2, 2)),
                                                                                 (8, 3, (nothing, 6)),
                                                                                 (8, 8, (nothing, 1)),
                                                                                 (8, 8, (1, 1)) ]
            test_ts = rand(T, src_size)
            buf_ts = if T <: AbstractOglBlock
                b = Buffer(block_byte_size(T) * src_size, true)
                for (i, t) in enumerate(test_ts)
                    set_buffer_data(b, test_ts[i], 1 + ((i-1)*block_byte_size(T)))
                end
                b
            else
                Buffer(true, test_ts)
            end
            test_ts2 = get_buffer_data(buf_ts, Vector{T},
                                       1 + block_byte_size(T) * (first_src_element - 1),
                                       fixed_element_count=dest_size)

            @bp_check(length(test_ts2) == dest_expected_size,
                      "Expected ", Vector{T}, " with ",
                        (src_size, first_src_element, (dest_size, dest_expected_size)),
                        ", got length of ", length(test_ts2))
            for i in 1:dest_expected_size
                @bp_check(test_ts[first_src_element + i - 1] ==
                            test_ts2[i],
                          "Expected:\n", test_ts[first_src_element + i - 1],
                          "\n\nGot:\n", test_ts2[i],
                          "\nSource array: \n", test_ts,
                          "\nTest: ", src_size, "/", first_src_element, "/", dest_size,
                          "\nDest array: \n", test_ts2)
            end
        end        
    end
end