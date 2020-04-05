bits 64
section .text
global start
start:
    lea rbx, [rel vs+0x10000]
    call w_main
    mov rax, 0x2000001
    mov rdi, 0
    syscall
w_trip:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    ret
w_rotr:
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_rotl:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    ret
w_over:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    ret
w_tuck:
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_nip:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_dup2:
    call w_over
    call w_over
    ret
w_drop2:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_swap2:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_rotr
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_rotr
    ret
w_dup3:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_dup2
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_rotr
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w__do_:
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    ret
w__do__dn_:
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    ret
w_0_dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_0_dm_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    ret
w_0_do_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do_
    ret
w_0_dm__dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    ret
w_0_do__dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do__dn_
    ret
w_1_dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_1_dm_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    ret
w_1_do_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do_
    ret
w_1_dm__dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    ret
w_1_do__dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do__dn_
    ret
w_1_cl_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_1_cn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    ret
w_2_ck_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_2_cp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    ret
w_2_cf_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    ret
w_2_cl_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_2_cn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    ret
w_not:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_and:
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    test rax, rax
    jz .L0
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L1
.L0:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L1:
    ret
w_or:
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    test rax, rax
    jz .L2
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    jmp .L3
.L2:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L3:
    ret
w_byte:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    ret
w_bytes:
    ret
w_byte_ea__ea_:
    add rax, [rbx]
    lea rbx, [rbx+8]
    mov al, [rax]
    movzx eax, al
    ret
w_byte_cb__cb_:
    add rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rax], cl
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_long:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8
    ret
w_longs:
    call w_long
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_long_ea__ea_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_longs
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rax]
    ret
w_long_cb__cb_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_longs
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w__cb__cb_:
    test rax, rax
    jz .L4
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L5
.L4:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+0]
    call w_panic_cb_
.L5:
    ret
w_panic_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+113]
    call w_str_cn_trace_cb_
    call w_str_cn_trace_cn_ln_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    mov rdi, rax
    mov rax, 0x2000001
    syscall
    ret
w__cb__cb__dn_:
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_
    ret
w__cb__cb_0:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__cb__cb__dn_
    ret
w__cb__cb_1:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__cb__cb__dn_
    ret
w__cb__cb_00:
    call w__cb__cb_0
    call w__cb__cb_0
    ret
w__cb__cb_01:
    call w__cb__cb_1
    call w__cb__cb_0
    ret
w__cb__cb_10:
    call w__cb__cb_0
    call w__cb__cb_1
    ret
w__cb__cb_11:
    call w__cb__cb_1
    call w__cb__cb_1
    ret
w__cb__cb_000:
    call w__cb__cb_0
    call w__cb__cb_0
    call w__cb__cb_0
    ret
w__cb__cb_001:
    call w__cb__cb_1
    call w__cb__cb_0
    call w__cb__cb_0
    ret
w__cb__cb_010:
    call w__cb__cb_0
    call w__cb__cb_1
    call w__cb__cb_0
    ret
w__cb__cb_011:
    call w__cb__cb_1
    call w__cb__cb_1
    call w__cb__cb_0
    ret
w__cb__cb_100:
    call w__cb__cb_0
    call w__cb__cb_0
    call w__cb__cb_1
    ret
w__cb__cb_101:
    call w__cb__cb_1
    call w__cb__cb_0
    call w__cb__cb_1
    ret
w__cb__cb_110:
    call w__cb__cb_0
    call w__cb__cb_1
    call w__cb__cb_1
    ret
w__cb__cb_111:
    call w__cb__cb_1
    call w__cb__cb_1
    call w__cb__cb_1
    ret
w_str_cn_head:
    mov al, [rax]
    movzx eax, al
    ret
w_str_cn_head_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_head
    ret
w_str_cn_tail:
    call w_str_cn_head_dp_
    test rax, rax
    jz .L6
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    jmp .L7
.L6:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L7:
    ret
w_str_cn_tail_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_tail
    ret
w_str_cn_null:
    call w_str_cn_head
    call w_0_dn_
    ret
w_str_cn_null_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_null
    ret
w_str_cn_not_cn_null:
    call w_str_cn_null
    call w_not
    ret
w_str_cn_not_cn_null_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_not_cn_null
    ret
w_str_cn_length_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_length
    ret
w_str_cn_length:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_str_cn_not_cn_null_dp_
.L8:
    test rax, rax
    jz .L9
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_tail
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_str_cn_not_cn_null_dp_
    jmp .L8
.L9:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_STR_fp_BUF_fp_SIZE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4096
    ret
w_str_cn_buf_cn_length_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF_fp_LEN]
    mov rax, [rax]
    ret
w_str_cn_buf_cn_length_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF_fp_LEN]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_str_cn_buf_cn_u8_cb_
    ret
w_str_cn_buf_cn_u8_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_byte_cb__cb_
    ret
w_str_cn_buf_cn_u8_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_byte_ea__ea_
    ret
w_str_cn_buf_cn_full_dp_:
    call w_str_cn_buf_cn_length_dp_
    call w_1_cl_
    call w_STR_fp_BUF_fp_SIZE
    call w__do__dn_
    ret
w_str_cn_buf_cn_clear_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_str_cn_buf_cn_length_cb_
    ret
w_str_cn_buf_cn_push_cb_:
    call w_str_cn_buf_cn_full_dp_
    test rax, rax
    jz .L10
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+17]
    call w_panic_cb_
    jmp .L11
.L10:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_length_dp_
    call w_str_cn_buf_cn_u8_cb_
    call w_str_cn_buf_cn_length_dp_
    call w_1_cl_
    call w_str_cn_buf_cn_length_cb_
.L11:
    ret
w_str_cn_buf_cn_write_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_str_cn_buf_cn_length_dp_
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000004
    syscall
    mov rax, [rbx+16]
    lea rbx, [rbx+24]
    ret
w_str_cn_buf_cn_print_cb_:
    call w_file_cn_out_ea_
    call w_str_cn_buf_cn_write_cb_
    ret
w_file_cn_out_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_OUT]
    mov rax, [rax]
    ret
w_str_cn_buf_cn_trace_cb_:
    call w_file_cn_err_ea_
    call w_str_cn_buf_cn_write_cb_
    ret
w_file_cn_err_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_ERR]
    mov rax, [rax]
    ret
w_str_cn_buf_cn_read_cb_:
    call w_str_cn_buf_cn_clear_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_STR_fp_BUF_fp_SIZE
    call w_1_cn_
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000003
    syscall
    lea rbx, [rbx+16]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L12
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+64]
    call w_panic_cb_
    jmp .L13
.L12:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_length_cb_
.L13:
    ret
w_str_cn_buf_cn_input_cb_:
    call w_file_cn_in_ea_
    call w_str_cn_buf_cn_read_cb_
    ret
w_file_cn_in_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_IN]
    mov rax, [rax]
    ret
w_str_cn_buf_cn_copy_cb_:
    call w_str_cn_buf_cn_clear_cb_
    call w_str_cn_head_dp_
.L14:
    test rax, rax
    jz .L15
    call w_str_cn_buf_cn_push_cb_
    call w_str_cn_tail
    call w_str_cn_buf_cn_full_dp_
    test rax, rax
    jz .L16
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    jmp .L17
.L16:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_head_dp_
.L17:
    jmp .L14
.L15:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_str_cn_buf_cb_:
    call w_str_cn_buf_cn_copy_cb_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_run_cn_tests:
    call w_test_cn_if
    call w_test_cn_drop
    call w_test_cn_dup
    call w_test_cn_swap
    call w_test_cn_dip
    call w_test_cn_trip
    call w_test_cn_rotr
    call w_test_cn_rotl
    call w_test_dn_
    call w_test_dm_
    call w_test_dm__dn_
    call w_test_do_
    call w_test_do__dn_
    call w_test_cl_
    call w_test_cn_
    call w_test_ck_
    call w_test_cp_
    call w_test_cf_
    call w_test_cn_str
    call w_test_cn_while
    call w_test_ea__cb_
    ret
w_test_cn_if:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__cb__cb_1
    call w__cb__cb_0
    ret
w_test_cn_drop:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w__cb__cb_0
    ret
w_test_cn_dup:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__cb__cb_011
    ret
w_test_cn_swap:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w__cb__cb_001
    ret
w_test_cn_dip:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w__cb__cb_010
    ret
w_test_cn_trip:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_trip
    call w__cb__cb_111
    call w__cb__cb_0
    ret
w_test_cn_rotr:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_rotr
    call w__cb__cb_010
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_rotr
    call w__cb__cb_001
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_rotr
    call w__cb__cb_100
    ret
w_test_cn_rotl:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_rotl
    call w__cb__cb_001
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_rotl
    call w__cb__cb_100
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_rotl
    call w__cb__cb_010
    ret
w_test_dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_1
    ret
w_test_dm_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w__cb__cb_0
    ret
w_test_dm__dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w__cb__cb_1
    ret
w_test_do_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__do_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__do_
    call w__cb__cb_0
    ret
w_test_do__dn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do__dn_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__do__dn_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do__dn_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__do__dn_
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__do__dn_
    call w__cb__cb_0
    ret
w_test_cl_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w__cb__cb_0
    ret
w_test_cn_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__cb__cb__dn_
    ret
w_test_ck_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    imul rax, [rbx]
    lea rbx, [rbx+8]
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8
    call w__cb__cb__dn_
    ret
w_test_cp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -5
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -4
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    call w__cb__cb__dn_
    ret
w_test_cf_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -5
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -4
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w__cb__cb_1
    ret
w_test_cn_str:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+85]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_head
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 65
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_tail
    call w_str_cn_head
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 97
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_tail
    call w_str_cn_tail
    call w_str_cn_head
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 66
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_tail
    call w_str_cn_tail
    call w_str_cn_tail
    call w_str_cn_head
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 98
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_tail
    call w_str_cn_tail
    call w_str_cn_tail
    call w_str_cn_tail
    call w_str_cn_head
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__cb__cb__dn_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+90]
    call w_str_cn_length
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+91]
    call w_str_cn_length
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+97]
    call w_str_cn_length
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 13
    call w__cb__cb__dn_
    ret
w_test_cn_while:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 999
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 20
.L18:
    test rax, rax
    jz .L19
    call w_1_cn_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    jmp .L18
.L19:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 30
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 999
    call w__cb__cb__dn_
    ret
w_test_ea__cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 99
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_cb__cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_ea__ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 99
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 30
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_cb__cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_ea__ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 99
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_ea__ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 30
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_long_cb__cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 99
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_cb__cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 99
    call w__cb__cb__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TEST_fp_BUF]
    call w_byte_ea__ea_
    call w__cb__cb_0
    ret
w_stdin:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    ret
w_stdout:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    ret
w_stderr:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    ret
w_init_cb_:
    call w_init_cn_io_cb_
    ret
w_init_cn_io_cb_:
    call w_stdin
    call w_file_cn_in_cb_
    call w_stdout
    call w_file_cn_out_cb_
    call w_stderr
    call w_file_cn_err_cb_
    ret
w_file_cn_in_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_IN]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_file_cn_out_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_OUT]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_file_cn_err_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_ERR]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_str_cn_write_cb_:
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_str_cn_not_cn_null_dp_
.L20:
    test rax, rax
    jz .L21
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_copy_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_write_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_str_cn_not_cn_null_dp_
    jmp .L20
.L21:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_drop2
    ret
w_str_cn_print_cb_:
    call w_file_cn_out_ea_
    call w_str_cn_write_cb_
    ret
w_str_cn_trace_cb_:
    call w_file_cn_err_ea_
    call w_str_cn_write_cb_
    ret
w_str_cn_print_cn_sp_cb_:
    call w_str_cn_print_cb_
    call w_print_cn_sp_cb_
    ret
w_print_cn_sp_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 32
    call w_print_cn_char_cb_
    ret
w_str_cn_trace_cn_sp_cb_:
    call w_str_cn_trace_cb_
    call w_trace_cn_sp_cb_
    ret
w_trace_cn_sp_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 32
    call w_trace_cn_char_cb_
    ret
w_str_cn_print_cn_ln_cb_:
    call w_str_cn_print_cb_
    call w_print_cn_ln_cb_
    ret
w_print_cn_ln_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    call w_print_cn_char_cb_
    ret
w_str_cn_trace_cn_ln_cb_:
    call w_str_cn_trace_cb_
    call w_trace_cn_ln_cb_
    ret
w_trace_cn_ln_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    call w_trace_cn_char_cb_
    ret
w_str_cn_buf_cn_char_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_str_cn_buf_cn_u8_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_str_cn_buf_cn_length_cb_
    ret
w_print_cn_char_cb_:
    call w_str_cn_buf_cn_char_cb_
    call w_str_cn_buf_cn_print_cb_
    ret
w_trace_cn_char_cb_:
    call w_str_cn_buf_cn_char_cb_
    call w_str_cn_buf_cn_trace_cb_
    ret
w_print_cn_quote_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 34
    call w_print_cn_char_cb_
    ret
w_trace_cn_quote_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 34
    call w_trace_cn_char_cb_
    ret
w_to_cn_digit:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 48
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_negate:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_abs:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_0_dm_
    test rax, rax
    jz .L22
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_negate
    jmp .L23
.L22:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L23:
    ret
w_str_cn_buf_cn_int_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_0_dn_
    test rax, rax
    jz .L24
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+111]
    call w_str_cn_buf_cb_
    jmp .L25
.L24:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_abs
    call w_str_cn_buf_cn_clear_cb_
.L26:
    test rax, rax
    jz .L27
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_to_cn_digit
    call w_str_cn_buf_cn_push_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    jmp .L26
.L27:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L28
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 45
    call w_str_cn_buf_cn_push_cb_
    jmp .L29
.L28:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L29:
    call w_str_cn_buf_cn_reverse_cb_
.L25:
    ret
w_str_cn_buf_cn_reverse_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_str_cn_buf_cn_length_dp_
    call w_1_cn_
    call w_dup2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L30:
    test rax, rax
    jz .L31
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_dup2
    call w_str_cn_buf_cn_swap_cn_u8_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_1_cn_
    call w_dup2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L30
.L31:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_drop2
    ret
w_int_cn_write_cb_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_int_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_str_cn_buf_cn_write_cb_
    ret
w_int_cn_print_cb_:
    call w_file_cn_out_ea_
    call w_int_cn_write_cb_
    ret
w_int_cn_trace_cb_:
    call w_file_cn_err_ea_
    call w_int_cn_write_cb_
    ret
w_int_cn_print_cn_sp_cb_:
    call w_int_cn_print_cb_
    call w_print_cn_sp_cb_
    ret
w_int_cn_trace_cn_sp_cb_:
    call w_int_cn_trace_cb_
    call w_trace_cn_sp_cb_
    ret
w_int_cn_print_cn_ln_cb_:
    call w_int_cn_print_cb_
    call w_print_cn_ln_cb_
    ret
w_int_cn_trace_cn_ln_cb_:
    call w_int_cn_trace_cb_
    call w_trace_cn_ln_cb_
    ret
w_str_cn_buf_cn_swap_cn_u8_cb_:
    call w_dup2
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_u8_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_u8_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_str_cn_buf_cn_u8_cb_
    call w_str_cn_buf_cn_u8_cb_
    ret
w_FILE_fp_BUF_fp_SIZE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 65536
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_file_cn_buf_cn_length_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_BUF_fp_LENGTH]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_BUF]
    call w_byte_cb__cb_
    ret
w_file_cn_buf_cn_length_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_BUF_fp_LENGTH]
    mov rax, [rax]
    ret
w_file_cn_buf_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_BUF]
    call w_byte_ea__ea_
    ret
w_str_cn_buf_cn_open_cn_file_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000005
    syscall
    lea rbx, [rbx+16]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L32
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+128]
    call w_panic_cb_
    jmp .L33
.L32:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L33:
    ret
w_str_cn_buf_cn_create_cn_file_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1537
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 438
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000005
    syscall
    lea rbx, [rbx+16]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L34
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+149]
    call w_panic_cb_
    jmp .L35
.L34:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L35:
    ret
w_str_cn_open_cn_file_cb_:
    call w_str_cn_buf_cb_
    call w_str_cn_buf_cn_open_cn_file_cb_
    ret
w_str_cn_create_cn_file_cb_:
    call w_str_cn_buf_cb_
    call w_str_cn_buf_cn_create_cn_file_cb_
    ret
w_read_cn_file_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_FILE_fp_BUF]
    call w_FILE_fp_BUF_fp_SIZE
    call w_1_cn_
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000003
    syscall
    lea rbx, [rbx+16]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L36
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+170]
    call w_panic_cb_
    jmp .L37
.L36:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_file_cn_buf_cn_length_cb_
   mov rdi, rax
   mov rax, 0x2000006
   syscall
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L38
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+192]
    call w_panic_cb_
    jmp .L39
.L38:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L39:
.L37:
    ret
w_read_cn_mirth_cn_src_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+213]
    call w_str_cn_trace_cn_ln_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+231]
    call w_str_cn_open_cn_file_cb_
    call w_read_cn_file_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+241]
    call w_str_cn_trace_cb_
    call w_file_cn_buf_cn_length_ea_
    call w_int_cn_trace_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+247]
    call w_str_cn_trace_cb_
    call w_FILE_fp_BUF_fp_SIZE
    call w_int_cn_trace_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+272]
    call w_str_cn_trace_cn_ln_cb_
    ret
w_MAX_fp_NAMES:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8192
    ret
w_num_cn_names_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NUM_fp_NAMES]
    mov rax, [rax]
    ret
w_num_cn_names_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NUM_fp_NAMES]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_NAME_fp_QUADS:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8
    ret
w_NAME_fp_SIZE:
    call w_NAME_fp_QUADS
    call w_longs
    ret
w_NAME_fp_BUF_fp_SIZE:
    call w_NAME_fp_SIZE
    call w_MAX_fp_NAMES
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_str_cn_buf_cn_recalc_cn_length_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_byte_ea__ea_
.L40:
    test rax, rax
    jz .L41
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_byte_ea__ea_
    jmp .L40
.L41:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_length_cb_
    ret
w_str_cn_pad_cn_zeros_cn_to_cn_length_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_length_dp_
    call w__do_
.L42:
    test rax, rax
    jz .L43
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_length_dp_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_length_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w__do_
    jmp .L42
.L43:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_name_cn_load_cb_:
    call w_name_cn_quads_cn_load_cb_
    call w_NAME_fp_SIZE
    call w_str_cn_buf_cn_length_cb_
    call w_str_cn_buf_cn_recalc_cn_length_cb_
    ret
w_name_cn_quads_cn_load_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 6
    call w_name_cn_quad_cn_load_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 7
    call w_name_cn_quad_cn_load_cb_
    ret
w_name_cn_quad_cn_eq:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_NAME_fp_SIZE
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NAME_fp_BUF]
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_tuck
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_long_ea__ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_long_ea__ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_name_cn_quads_cn_eq:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L44
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L46
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L48
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L50
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L52
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L54
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 6
    call w_name_cn_quad_cn_eq
    test rax, rax
    jz .L56
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 7
    call w_name_cn_quad_cn_eq
    jmp .L57
.L56:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L57:
    jmp .L55
.L54:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L55:
    jmp .L53
.L52:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L53:
    jmp .L51
.L50:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L51:
    jmp .L49
.L48:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L49:
    jmp .L47
.L46:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L47:
    jmp .L45
.L44:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L45:
    ret
w_name_cn_quads_cn_eq_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_quads_cn_eq
    ret
w_name_cn_quad_cn_save_cb_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_NAME_fp_SIZE
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NAME_fp_BUF]
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_tuck
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_long_ea__ea_
    call w_rotr
    call w_long_cb__cb_
    ret
w_name_cn_quads_cn_save_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 6
    call w_name_cn_quad_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 7
    call w_name_cn_quad_cn_save_cb_
    ret
w_name_cn_quad_cn_load_cb_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_NAME_fp_SIZE
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NAME_fp_BUF]
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_tuck
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_long_ea__ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STR_fp_BUF]
    call w_long_cb__cb_
    ret
w_name_cn_save_cb_:
    call w_NAME_fp_SIZE
    call w_1_cl_
    call w_str_cn_pad_cn_zeros_cn_to_cn_length_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L58:
    test rax, rax
    jz .L59
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_name_cn_quads_cn_eq_dp_
    test rax, rax
    jz .L60
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    jmp .L61
.L60:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L61:
    jmp .L58
.L59:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L62
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L63
.L62:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_quads_cn_save_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_num_cn_names_cb_
.L63:
    ret
w_show_cn_names_cn_table_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L64:
    test rax, rax
    jz .L65
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_int_cn_print_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+275]
    call w_str_cn_print_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_load_cb_
    call w_str_cn_buf_cn_print_cb_
    call w_print_cn_ln_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L64
.L65:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_PRIM_fp_END:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    ret
w_PRIM_fp_ID:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    ret
w_PRIM_fp_DUP:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    ret
w_PRIM_fp_DROP:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    ret
w_PRIM_fp_SWAP:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    ret
w_PRIM_fp_DIP:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    ret
w_PRIM_fp_IF:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 6
    ret
w_PRIM_fp_WHILE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 7
    ret
w_PRIM_fp_INT_fp_ADD:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8
    ret
w_PRIM_fp_INT_fp_SUB:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 9
    ret
w_PRIM_fp_INT_fp_MUL:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    ret
w_PRIM_fp_INT_fp_DIV:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 11
    ret
w_PRIM_fp_INT_fp_MOD:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 12
    ret
w_PRIM_fp_INT_fp_EQ:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 13
    ret
w_PRIM_fp_INT_fp_LT:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 14
    ret
w_PRIM_fp_INT_fp_LE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 15
    ret
w_PRIM_fp_MEM_fp_GET:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 16
    ret
w_PRIM_fp_MEM_fp_SET:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 17
    ret
w_PRIM_fp_MEM_fp_GET_fp_BYTE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 18
    ret
w_PRIM_fp_MEM_fp_SET_fp_BYTE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 19
    ret
w_PRIM_fp_POSIX_fp_READ:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 20
    ret
w_PRIM_fp_POSIX_fp_WRITE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 21
    ret
w_PRIM_fp_POSIX_fp_OPEN:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 22
    ret
w_PRIM_fp_POSIX_fp_CLOSE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 23
    ret
w_PRIM_fp_POSIX_fp_EXIT:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 24
    ret
w_PRIM_fp_POSIX_fp_MMAP:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 25
    ret
w_PRIM_fp_DEBUG:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 26
    ret
w_PRIM_fp_MIRTH_fp_REVISION:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 27
    ret
w_PRIM_fp_DEF:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 28
    ret
w_PRIM_fp_DEF_fp_STATIC_fp_BUFFER:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 29
    ret
w_PRIM_fp_OUTPUT_fp_ASM:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 30
    ret
w_PRIM_fp_OUTPUT_fp_C99:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 31
    ret
w_NUM_fp_PRIMS:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 32
    ret
w_is_cn_prim_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_NUM_fp_PRIMS
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    ret
w_def_cn_prim_cb_:
    call w_str_cn_buf_cb_
    call w_name_cn_save_cb_
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w__cb__cb__dn_
    ret
w_init_cn_names_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_num_cn_names_cb_
    call w_PRIM_fp_END
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+278]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_ID
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+282]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_DUP
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+285]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_DROP
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+289]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_SWAP
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+294]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_DIP
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+299]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_IF
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+303]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_WHILE
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+306]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_ADD
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+312]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_SUB
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+320]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_MUL
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+322]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_DIV
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+324]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_MOD
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+326]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_EQ
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+328]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_LT
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+330]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_INT_fp_LE
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+332]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_MEM_fp_GET
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+335]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_MEM_fp_SET
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+337]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_MEM_fp_GET_fp_BYTE
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+339]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_MEM_fp_SET_fp_BYTE
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+345]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_POSIX_fp_READ
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+351]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_POSIX_fp_WRITE
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+363]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_POSIX_fp_OPEN
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+376]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_POSIX_fp_CLOSE
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+388]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_POSIX_fp_EXIT
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+401]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_POSIX_fp_MMAP
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+413]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_DEBUG
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+425]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_MIRTH_fp_REVISION
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+428]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_DEF
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+448]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_DEF_fp_STATIC_fp_BUFFER
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+452]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_OUTPUT_fp_ASM
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+470]
    call w_def_cn_prim_cb_
    call w_PRIM_fp_OUTPUT_fp_C99
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+481]
    call w_def_cn_prim_cb_
    ret
w_MAX_fp_STRINGS:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 32768
    ret
w_strings_cn_size_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STRINGS_fp_SIZE]
    mov rax, [rax]
    ret
w_strings_cn_size_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STRINGS_fp_SIZE]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_strings_cn_u8_ea_:
    mov al, [rax]
    movzx eax, al
    ret
w_strings_cn_u8_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_strings_cn_u8_ea_
    ret
w_strings_cn_u8_cb_:
    mov rcx, [rbx]
    mov [rax], cl
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_strings_cn_push_cb_:
    call w_strings_cn_size_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STRINGS_fp_BUF]
    call w_byte_cb__cb_
    call w_strings_cn_size_ea_
    call w_1_cl_
    call w_strings_cn_size_cb_
    ret
w_strings_cn_save_cb_:
    call w_strings_cn_size_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_length_dp_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L66:
    test rax, rax
    jz .L67
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_u8_ea_
    call w_strings_cn_push_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_length_dp_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L66
.L67:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_strings_cn_push_cb_
    ret
w_strings_cn_load_cb_:
    call w_str_cn_buf_cn_clear_cb_
    call w_strings_cn_u8_dp_
.L68:
    test rax, rax
    jz .L69
    call w_str_cn_buf_cn_push_cb_
    call w_1_cl_
    call w_strings_cn_u8_dp_
    jmp .L68
.L69:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_strings_cn_load_cn_escaped_cb_:
    call w_strings_cn_load_cb_
    ret
w_MAX_fp_TOKENS:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 32768
    ret
w_num_cn_tokens_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NUM_fp_TOKENS]
    mov rax, [rax]
    ret
w_num_cn_tokens_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NUM_fp_TOKENS]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_clear_cn_tokens_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_num_cn_tokens_cb_
    ret
w_token_cn_type_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_TYPE]
    call w_byte_cb__cb_
    ret
w_token_cn_type_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_TYPE]
    call w_byte_ea__ea_
    ret
w_token_cn_type_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_type_ea_
    ret
w_TOKEN_fp_NONE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    ret
w_TOKEN_fp_LPAREN:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    ret
w_TOKEN_fp_RPAREN:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    ret
w_TOKEN_fp_COMMA:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    ret
w_TOKEN_fp_NAME:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4
    ret
w_TOKEN_fp_INT:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 5
    ret
w_TOKEN_fp_STR:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 6
    ret
w_token_cn_type_cn_str:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_NONE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L70
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+492]
    jmp .L71
.L70:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L72
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+497]
    jmp .L73
.L72:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_RPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L74
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+504]
    jmp .L75
.L74:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_COMMA
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L76
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+512]
    jmp .L77
.L76:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L78
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+518]
    jmp .L79
.L78:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_INT
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L80
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+523]
    jmp .L81
.L80:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_TOKEN_fp_STR
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L82
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+527]
    jmp .L83
.L82:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+531]
.L83:
.L81:
.L79:
.L77:
.L75:
.L73:
.L71:
    ret
w_token_cn_type_cn_print_cb_:
    call w_token_cn_type_cn_str
    call w_str_cn_print_cb_
    ret
w_token_cn_value_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_VALUE]
    call w_long_cb__cb_
    ret
w_token_cn_value_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_VALUE]
    call w_long_ea__ea_
    ret
w_token_cn_value_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_value_ea_
    ret
w_token_cn_row_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_ROW]
    call w_long_cb__cb_
    ret
w_token_cn_row_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_ROW]
    call w_long_ea__ea_
    ret
w_token_cn_row_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_row_ea_
    ret
w_token_cn_col_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_COL]
    call w_long_cb__cb_
    ret
w_token_cn_col_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_TOKEN_fp_COL]
    call w_long_ea__ea_
    ret
w_token_cn_col_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_col_ea_
    ret
w_token_cn_new:
    call w_num_cn_tokens_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_num_cn_tokens_cb_
    ret
w_token_cn_trace_cn_prefix_cb_:
    call w_token_cn_row_dp_
    call w_int_cn_trace_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+545]
    call w_str_cn_trace_cb_
    call w_token_cn_col_dp_
    call w_int_cn_trace_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+547]
    call w_str_cn_trace_cn_sp_cb_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_token_cn_print_cn_prefix_cb_:
    call w_token_cn_row_dp_
    call w_int_cn_print_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+549]
    call w_str_cn_print_cb_
    call w_token_cn_col_dp_
    call w_int_cn_print_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+551]
    call w_str_cn_print_cn_sp_cb_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_token_cn_print_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_print_cn_prefix_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_int_cn_print_cn_sp_cb_
    call w_token_cn_type_dp_
    call w_token_cn_type_cn_print_cb_
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L84
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_print_cn_sp_cb_
    call w_token_cn_value_dp_
    call w_name_cn_load_cb_
    call w_str_cn_buf_cn_print_cb_
    jmp .L85
.L84:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_STR
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L86
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_print_cn_sp_cb_
    call w_print_cn_quote_cb_
    call w_token_cn_value_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STRINGS_fp_BUF]
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w_strings_cn_load_cn_escaped_cb_
    call w_str_cn_buf_cn_print_cb_
    call w_print_cn_quote_cb_
    jmp .L87
.L86:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_print_cn_sp_cb_
    call w_token_cn_value_dp_
    call w_int_cn_print_cb_
.L87:
.L85:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_print_cn_ln_cb_
    ret
w_show_cn_tokens_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_tokens_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L88:
    test rax, rax
    jz .L89
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_print_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_tokens_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L88
.L89:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_lexer_cn_row_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_ROW]
    mov rax, [rax]
    ret
w_lexer_cn_row_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_ROW]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_lexer_cn_col_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_COL]
    mov rax, [rax]
    ret
w_lexer_cn_col_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_COL]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_lexer_cn_idx_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_IDX]
    mov rax, [rax]
    ret
w_lexer_cn_idx_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_IDX]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_LEXER_fp_STACK_fp_SIZE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 256
    ret
w_lexer_cn_stack_cn_length_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_STACK_fp_LENGTH]
    mov rax, [rax]
    ret
w_lexer_cn_stack_cn_length_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_STACK_fp_LENGTH]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_lexer_cn_stack_cn_clear_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_stack_cn_length_cb_
    ret
w_lexer_cn_stack_cn_empty_dp_:
    call w_lexer_cn_stack_cn_length_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    ret
w_lexer_cn_stack_cn_full_dp_:
    call w_lexer_cn_stack_cn_length_ea_
    call w_LEXER_fp_STACK_fp_SIZE
    call w__do__dn_
    ret
w_lexer_cn_stack_cn_push_cb_:
    call w_lexer_cn_stack_cn_full_dp_
    test rax, rax
    jz .L90
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+553]
    call w_panic_cb_
    jmp .L91
.L90:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_stack_cn_length_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_lexer_cn_stack_cn_length_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_STACK_fp_BUF]
    call w_long_cb__cb_
.L91:
    ret
w_lexer_cn_stack_cn_pop_cb_:
    call w_lexer_cn_stack_cn_empty_dp_
    test rax, rax
    jz .L92
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+590]
    call w_panic_cb_
    jmp .L93
.L92:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_stack_cn_length_ea_
    call w_1_cn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_lexer_cn_stack_cn_length_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_LEXER_fp_STACK_fp_BUF]
    call w_long_ea__ea_
.L93:
    ret
w_run_cn_lexer_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_lexer_cn_row_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_lexer_cn_col_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_idx_cb_
    call w_lexer_cn_done_dp_
    call w_not
.L94:
    test rax, rax
    jz .L95
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_next_cb_
    call w_lexer_cn_done_dp_
    call w_not
    jmp .L94
.L95:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_TOKEN_fp_NONE
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_emit_cb_
    ret
w_lexer_cn_done_dp_:
    call w_lexer_cn_idx_ea_
    call w_file_cn_buf_cn_length_ea_
    call w__do__dn_
    ret
w_lexer_cn_next_cb_:
    call w_lexer_cn_peek
    call w_is_cn_newline_dp_
    test rax, rax
    jz .L96
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_emit_cn_newline_cb_
    jmp .L97
.L96:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_whitespace_dp_
    test rax, rax
    jz .L98
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L99
.L98:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_pound_dp_
    test rax, rax
    jz .L100
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_skip_cn_comment_cb_
    jmp .L101
.L100:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_comma_dp_
    test rax, rax
    jz .L102
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_TOKEN_fp_COMMA
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_emit_cb_
    jmp .L103
.L102:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_lparen_dp_
    test rax, rax
    jz .L104
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_emit_cn_lparen_cb_
    jmp .L105
.L104:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_rparen_dp_
    test rax, rax
    jz .L106
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_emit_cn_rparen_cb_
    jmp .L107
.L106:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_quote_dp_
    test rax, rax
    jz .L108
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_emit_cn_string_cb_
    jmp .L109
.L108:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_name_cn_char_dp_
    test rax, rax
    jz .L110
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_emit_cn_name_cb_
    jmp .L111
.L110:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L111:
.L109:
.L107:
.L105:
.L103:
.L101:
.L99:
.L97:
    call w_lexer_cn_move_cb_
    ret
w_lexer_cn_emit_cb_:
    call w_lexer_cn_make_cb_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_lexer_cn_make_cb_:
    call w_token_cn_new
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_row_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_row_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_col_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_col_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_lexer_cn_peek:
    call w_lexer_cn_idx_ea_
    call w_file_cn_buf_ea_
    ret
w_is_cn_newline_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_lexer_cn_emit_cn_newline_cb_:
    call w_lexer_cn_row_ea_
    call w_1_cl_
    call w_lexer_cn_row_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_col_cb_
    ret
w_is_cn_whitespace_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 9
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 32
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_is_cn_pound_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 35
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_lexer_cn_skip_cn_comment_cb_:
    call w_lexer_cn_comment_cn_end_dp_
    call w_not
.L112:
    test rax, rax
    jz .L113
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_move_cb_
    call w_lexer_cn_comment_cn_end_dp_
    call w_not
    jmp .L112
.L113:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_peek
    call w_is_cn_newline_dp_
    test rax, rax
    jz .L114
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_emit_cn_newline_cb_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L115
.L114:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L115:
    ret
w_is_cn_comma_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 44
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_is_cn_lparen_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 40
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_lexer_cn_emit_cn_lparen_cb_:
    call w_TOKEN_fp_LPAREN
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_make_cb_
    call w_lexer_cn_stack_cn_push_cb_
    ret
w_is_cn_rparen_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 41
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_lexer_cn_emit_cn_rparen_cb_:
    call w_TOKEN_fp_RPAREN
    call w_lexer_cn_stack_cn_pop_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_make_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_token_cn_value_cb_
    ret
w_is_cn_quote_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 34
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_lexer_cn_emit_cn_string_cb_:
    call w_str_cn_buf_cn_clear_cb_
    call w_TOKEN_fp_STR
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_make_cb_
    call w_lexer_cn_move_cb_
    call w_lexer_cn_peek
    call w_is_cn_string_cn_end_dp_
    call w_not
.L116:
    test rax, rax
    jz .L117
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_push_cn_string_cn_char_cb_
    call w_lexer_cn_move_cb_
    call w_lexer_cn_peek
    call w_is_cn_string_cn_end_dp_
    call w_not
    jmp .L116
.L117:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_strings_cn_save_cb_
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_token_cn_value_cb_
    ret
w_is_cn_name_cn_char_dp_:
    call w_is_cn_special_cn_char_dp_
    test rax, rax
    jz .L118
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    jmp .L119
.L118:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 33
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 126
    call w_in_cn_range
.L119:
    ret
w_lexer_cn_emit_cn_name_cb_:
    call w_str_cn_buf_cn_clear_cb_
    call w_TOKEN_fp_NAME
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_lexer_cn_make_cb_
    call w_lexer_cn_peek
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
.L120:
    test rax, rax
    jz .L121
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_push_cb_
    call w_lexer_cn_move_cb_
    call w_lexer_cn_peek
    call w_is_cn_name_cn_char_dp_
    jmp .L120
.L121:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_move_cn_back_cb_
    call w_str_cn_buf_cn_is_cn_int_dp_
    test rax, rax
    jz .L122
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_int_dp_
    call w_over
    call w_token_cn_value_cb_
    call w_TOKEN_fp_INT
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_token_cn_type_cb_
    jmp .L123
.L122:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_name_cn_save_cb_
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_token_cn_value_cb_
.L123:
    ret
w_lexer_cn_move_cb_:
    call w_lexer_cn_idx_ea_
    call w_1_cl_
    call w_lexer_cn_idx_cb_
    call w_lexer_cn_col_ea_
    call w_1_cl_
    call w_lexer_cn_col_cb_
    ret
w_lexer_cn_move_cn_back_cb_:
    call w_lexer_cn_idx_ea_
    call w_1_cn_
    call w_lexer_cn_idx_cb_
    call w_lexer_cn_col_ea_
    call w_1_cn_
    call w_lexer_cn_col_cb_
    ret
w_str_cn_buf_cn_is_cn_int_dp_:
    call w_str_cn_buf_cn_is_cn_dec_cn_int_dp_
    ret
w_str_cn_buf_cn_int_dp_:
    call w_str_cn_buf_cn_dec_cn_int_dp_
    ret
w_str_cn_buf_cn_is_cn_dec_cn_int_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_u8_ea_
    call w_is_cn_sign_dp_
    call w_nip
    test rax, rax
    jz .L124
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    jmp .L125
.L124:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L125:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_u8_ea_
    call w_is_cn_digit_dp_
    call w_nip
.L126:
    test rax, rax
    jz .L127
    mov rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_u8_ea_
    call w_is_cn_digit_dp_
    call w_nip
    jmp .L126
.L127:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w__do__dn_
    test rax, rax
    jz .L128
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_length_dp_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    jmp .L129
.L128:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
.L129:
    ret
w_is_cn_sign_dp_:
    call w_is_cn_plus_cn_sign_dp_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_minus_cn_sign_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_is_cn_digit_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 48
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 57
    call w_in_cn_range
    ret
w_str_cn_buf_cn_dec_cn_int_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_u8_ea_
    call w_is_cn_sign_dp_
    test rax, rax
    jz .L130
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_minus_cn_sign_dp_
    call w_nip
    test rax, rax
    jz .L132
    mov rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    jmp .L133
.L132:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L133:
    call w_1_cl_
    jmp .L131
.L130:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L131:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_length_dp_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L134:
    test rax, rax
    jz .L135
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_str_cn_buf_cn_u8_ea_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 10
    imul rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 48
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_str_cn_buf_cn_length_dp_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L134
.L135:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_is_cn_minus_cn_sign_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 45
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_is_cn_string_cn_end_dp_:
    call w_is_cn_quote_dp_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_newline_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_nul_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_lexer_cn_push_cn_string_cn_char_cb_:
    call w_str_cn_buf_cn_push_cb_
    ret
w_lexer_cn_comment_cn_end_dp_:
    call w_lexer_cn_done_dp_
    test rax, rax
    jz .L136
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    jmp .L137
.L136:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_lexer_cn_peek
    call w_is_cn_newline_dp_
    call w_nip
.L137:
    ret
w_in_cn_range:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_over
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w__do__dn_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_is_cn_upper_cn_hexdigit_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 65
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 70
    call w_in_cn_range
    ret
w_is_cn_lower_cn_hexdigit_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 97
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 102
    call w_in_cn_range
    ret
w_is_cn_hexdigit_dp_:
    call w_is_cn_digit_dp_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_upper_cn_hexdigit_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_lower_cn_hexdigit_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_is_cn_nul_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_0_dn_
    ret
w_is_cn_plus_cn_sign_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 43
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_is_cn_special_cn_char_dp_:
    call w_is_cn_quote_dp_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_lparen_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_rparen_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_comma_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_is_cn_pound_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    add rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_VSTACK_fp_SIZE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8192
    ret
w_vstack_cn_len_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_VSTACK_fp_LEN]
    mov rax, [rax]
    ret
w_vstack_cn_len_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_VSTACK_fp_LEN]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_vstack_cn_empty_dp_:
    call w_vstack_cn_len_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    ret
w_vstack_cn_full_dp_:
    call w_vstack_cn_len_ea_
    call w_VSTACK_fp_SIZE
    call w__do__dn_
    ret
w_vstack_cn_i64_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_VSTACK_fp_BUF]
    call w_long_ea__ea_
    ret
w_vstack_cn_i64_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_VSTACK_fp_BUF]
    call w_long_cb__cb_
    ret
w_vstack_cn_pop_cb_:
    call w_vstack_cn_empty_dp_
    test rax, rax
    jz .L138
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+628]
    call w_panic_cb_
    jmp .L139
.L138:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_len_ea_
    call w_1_cn_
    call w_vstack_cn_len_cb_
    call w_vstack_cn_len_ea_
    call w_vstack_cn_i64_ea_
.L139:
    ret
w_vstack_cn_pop2_cb_:
    call w_vstack_cn_pop_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_pop_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_vstack_cn_pop3_cb_:
    call w_vstack_cn_pop_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_pop2_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_vstack_cn_pop4_cb_:
    call w_vstack_cn_pop_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_pop3_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_vstack_cn_pop5_cb_:
    call w_vstack_cn_pop_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_pop4_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_vstack_cn_pop6_cb_:
    call w_vstack_cn_pop_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_pop5_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    ret
w_vstack_cn_push_cb_:
    call w_vstack_cn_full_dp_
    test rax, rax
    jz .L140
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+678]
    call w_panic_cb_
    jmp .L141
.L140:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_len_ea_
    call w_vstack_cn_i64_cb_
    call w_vstack_cn_len_ea_
    call w_1_cl_
    call w_vstack_cn_len_cb_
.L141:
    ret
w_vstack_cn_push2_cb_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_push_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_vstack_cn_push_cb_
    ret
w_vstack_cn_push3_cb_:
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_push2_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_vstack_cn_push_cb_
    ret
w_vstack_cn_top_ea_:
    call w_vstack_cn_empty_dp_
    test rax, rax
    jz .L142
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+727]
    call w_panic_cb_
    jmp .L143
.L142:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_len_ea_
    call w_1_cn_
    call w_vstack_cn_i64_ea_
.L143:
    ret
w_vstack_cn_top_cb_:
    call w_vstack_cn_empty_dp_
    test rax, rax
    jz .L144
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+787]
    call w_panic_cb_
    jmp .L145
.L144:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_len_ea_
    call w_1_cn_
    call w_vstack_cn_i64_cb_
.L145:
    ret
w_vstack_cn_trace_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_vstack_cn_len_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L146:
    test rax, rax
    jz .L147
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_vstack_cn_i64_ea_
    call w_int_cn_trace_cn_sp_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_vstack_cn_len_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L146
.L147:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_trace_cn_ln_cb_
    ret
w_emit_cn_warning_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+846]
    call w_str_cn_trace_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_trace_cn_prefix_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+857]
    call w_str_cn_trace_cb_
    call w_str_cn_trace_cn_ln_cb_
    ret
w_emit_cn_error_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+867]
    call w_str_cn_trace_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_trace_cn_prefix_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+878]
    call w_str_cn_trace_cb_
    call w_str_cn_trace_cn_ln_cb_
    ret
w_emit_cn_fatal_cn_error_cb_:
    call w_emit_cn_error_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    mov rdi, rax
    mov rax, 0x2000001
    syscall
    ret
w_token_cn_run_cb_:
    call w_token_cn_run_cn_end_dp_
    call w_not
.L148:
    test rax, rax
    jz .L149
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_run_cn_one_cb_
    call w_token_cn_run_cn_end_dp_
    call w_not
    jmp .L148
.L149:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_token_cn_run_cn_end_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_tokens_ea_
    call w__do__dn_
    test rax, rax
    jz .L150
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    jmp .L151
.L150:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NONE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L152
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    jmp .L153
.L152:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_COMMA
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L154
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    jmp .L155
.L154:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_RPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
.L155:
.L153:
.L151:
    ret
w_token_cn_run_cn_one_cb_:
    call w_token_cn_type_dp_
    call w_TOKEN_fp_INT
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L156
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_dp_
    call w_vstack_cn_push_cb_
    jmp .L157
.L156:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_STR
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L158
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STRINGS_fp_BUF]
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_push_cb_
    jmp .L159
.L158:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L160
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_ea_
    jmp .L161
.L160:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L162
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_run_cn_name_cb_
    jmp .L163
.L162:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+886]
    call w_emit_cn_fatal_cn_error_cb_
.L163:
.L161:
.L159:
.L157:
    call w_1_cl_
    ret
w_token_cn_run_cn_name_cb_:
    call w_token_cn_value_dp_
    call w_is_cn_prim_dp_
    test rax, rax
    jz .L164
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_run_cn_prim_cb_
    jmp .L165
.L164:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_name_cn_undefined_dp_
    test rax, rax
    jz .L166
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+922]
    call w_emit_cn_fatal_cn_error_cb_
    jmp .L167
.L166:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_name_cn_is_cn_word_dp_
    test rax, rax
    jz .L168
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_nip
    call w_name_cn_value_ea_
    call w_token_cn_run_cb_
    jmp .L169
.L168:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_name_cn_is_cn_buffer_dp_
    test rax, rax
    jz .L170
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_nip
    call w_name_cn_value_ea_
    call w_buffer_cn_base_ea_
    call w_vstack_cn_push_cb_
    jmp .L171
.L170:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_drop2
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+941]
    call w_emit_cn_fatal_cn_error_cb_
.L171:
.L169:
.L167:
.L165:
    ret
w_token_cn_run_cn_prim_cb_:
    call w_token_cn_value_dp_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_ID
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L172
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    jmp .L173
.L172:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_SWAP
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L174
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_vstack_cn_push2_cb_
    jmp .L175
.L174:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_DUP
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L176
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_vstack_cn_push2_cb_
    jmp .L177
.L176:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_DROP
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L178
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop_cb_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L179
.L178:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_DIP
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L180
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_1
    call w_vstack_cn_pop_cb_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    call w_vstack_cn_push_cb_
    jmp .L181
.L180:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_IF
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L182
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_2
    call w_vstack_cn_pop_cb_
    test rax, rax
    jz .L184
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L185
.L184:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_nip
.L185:
    call w_token_cn_run_cb_
    jmp .L183
.L182:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_WHILE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L186
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_1
    call w_vstack_cn_pop_cb_
.L188:
    test rax, rax
    jz .L189
    call w_vstack_cn_push_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_token_cn_run_cb_
    call w_vstack_cn_pop_cb_
    jmp .L188
.L189:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L187
.L186:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_ADD
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L190
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_push_cb_
    jmp .L191
.L190:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_SUB
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L192
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w_vstack_cn_push_cb_
    jmp .L193
.L192:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_MUL
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L194
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    imul rax, [rbx]
    lea rbx, [rbx+8]
    call w_vstack_cn_push_cb_
    jmp .L195
.L194:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_DIV
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L196
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    call w_vstack_cn_push_cb_
    jmp .L197
.L196:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_MOD
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L198
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    call w_vstack_cn_push_cb_
    jmp .L199
.L198:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_EQ
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L200
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    call w_vstack_cn_push_cb_
    jmp .L201
.L200:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_LT
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L202
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    call w_vstack_cn_push_cb_
    jmp .L203
.L202:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_INT_fp_LE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L204
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    call w_vstack_cn_push_cb_
    jmp .L205
.L204:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_MEM_fp_GET
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L206
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop_cb_
    mov rax, [rax]
    call w_vstack_cn_push_cb_
    jmp .L207
.L206:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_MEM_fp_SET
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L208
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    jmp .L209
.L208:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_MEM_fp_GET_fp_BYTE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L210
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop_cb_
    mov al, [rax]
    movzx eax, al
    call w_vstack_cn_push_cb_
    jmp .L211
.L210:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_MEM_fp_SET_fp_BYTE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L212
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop2_cb_
    mov rcx, [rbx]
    mov [rax], cl
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    jmp .L213
.L212:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_POSIX_fp_MMAP
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L214
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 6
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop6_cb_
    mov rdi, [rbx+32]
    mov rsi, [rbx+24]
    mov rdx, [rbx+16]
    mov rcx, [rbx+8]
    mov r8, [rbx]
    mov r9, rax
    mov rax, 0x20000C5
    syscall
    lea rbx, [rbx+40]
    call w_vstack_cn_push_cb_
    jmp .L215
.L214:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_POSIX_fp_WRITE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L216
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop3_cb_
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000004
    syscall
    mov rax, [rbx+16]
    lea rbx, [rbx+24]
    jmp .L217
.L216:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_POSIX_fp_READ
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L218
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop3_cb_
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000003
    syscall
    lea rbx, [rbx+16]
    call w_vstack_cn_push_cb_
    jmp .L219
.L218:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_POSIX_fp_OPEN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L220
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop3_cb_
    mov rdi, [rbx+8]
    mov rsi, [rbx]
    mov rdx, rax
    mov rax, 0x2000005
    syscall
    lea rbx, [rbx+16]
    call w_vstack_cn_push_cb_
    jmp .L221
.L220:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_POSIX_fp_CLOSE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L222
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop_cb_
   mov rdi, rax
   mov rax, 0x2000006
   syscall
    call w_vstack_cn_push_cb_
    jmp .L223
.L222:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_POSIX_fp_EXIT
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L224
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    call w_vstack_cn_pop_cb_
    mov rdi, rax
    mov rax, 0x2000001
    syscall
    jmp .L225
.L224:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_DEBUG
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L226
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+969]
    call w_str_cn_trace_cb_
    call w_vstack_cn_trace_cb_
    jmp .L227
.L226:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_DEF
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L228
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_3
    call w_nip
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L230
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_dp_
    call w_name_cn_undefined_dp_
    test rax, rax
    jz .L232
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_DEF_fp_WORD
    call w_over
    call w_name_cn_sort_cb_
    call w_nip
    call w_name_cn_value_cb_
    jmp .L233
.L232:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+973]
    call w_emit_cn_fatal_cn_error_cb_
.L233:
    jmp .L231
.L230:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+994]
    call w_emit_cn_fatal_cn_error_cb_
.L231:
    jmp .L229
.L228:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_DEF_fp_STATIC_fp_BUFFER
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L234
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_1
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L236
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_dp_
    call w_name_cn_undefined_dp_
    test rax, rax
    jz .L238
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_nip
    call w_DEF_fp_BUFFER
    call w_over
    call w_name_cn_sort_cb_
    call w_vstack_cn_pop_cb_
    call w_buffer_cn_alloc_cb_
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    call w_name_cn_value_cb_
    jmp .L239
.L238:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1013]
    call w_emit_cn_fatal_cn_error_cb_
.L239:
    jmp .L237
.L236:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1036]
    call w_emit_cn_fatal_cn_error_cb_
.L237:
    jmp .L235
.L234:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_MIRTH_fp_REVISION
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L240
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    call w_vstack_cn_push_cb_
    jmp .L241
.L240:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_PRIM_fp_OUTPUT_fp_C99
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L242
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_arity_cn_check_cb_
    call w_token_cn_args_cn_1
    call w_vstack_cn_pop_cb_
    call w_run_cn_output_cn_c99_cb_
    jmp .L243
.L242:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1057]
    call w_emit_cn_warning_cb_
.L243:
.L241:
.L235:
.L229:
.L227:
.L225:
.L223:
.L221:
.L219:
.L217:
.L215:
.L213:
.L211:
.L209:
.L207:
.L205:
.L203:
.L201:
.L199:
.L197:
.L195:
.L193:
.L191:
.L187:
.L183:
.L181:
.L179:
.L177:
.L175:
.L173:
    ret
w_name_cn_undefined_dp_:
    call w_name_cn_sort_dp_
    call w_DEF_fp_NONE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_name_cn_is_cn_word_dp_:
    call w_name_cn_sort_dp_
    call w_DEF_fp_WORD
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_name_cn_value_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_DEF_fp_VALUE]
    call w_long_ea__ea_
    ret
w_name_cn_is_cn_buffer_dp_:
    call w_name_cn_sort_dp_
    call w_DEF_fp_BUFFER
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_buffer_cn_base_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_BUFFER_fp_BASE]
    call w_long_ea__ea_
    ret
w_arity_cn_check_cb_:
    call w_drop2
    ret
w_token_cn_args_cn_0:
    call w_1_cl_
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L244
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1088]
    call w_emit_cn_fatal_cn_error_cb_
    jmp .L245
.L244:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L245:
    ret
w_token_cn_args_cn_1:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L246
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_tuck
    call w_token_cn_next_cn_arg_cn_end
    call w_token_cn_type_dp_
    call w_TOKEN_fp_RPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L248
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_drop2
    jmp .L249
.L248:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1105]
    call w_emit_cn_fatal_cn_error_cb_
.L249:
    jmp .L247
.L246:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1134]
    call w_emit_cn_fatal_cn_error_cb_
.L247:
    ret
w_token_cn_args_cn_2:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L250
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_tuck
    call w_token_cn_next_cn_arg_cn_end
    call w_token_cn_type_dp_
    call w_TOKEN_fp_COMMA
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L252
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_tuck
    call w_token_cn_next_cn_arg_cn_end
    call w_token_cn_type_dp_
    call w_TOKEN_fp_RPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L254
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_drop2
    jmp .L255
.L254:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1159]
    call w_emit_cn_fatal_cn_error_cb_
.L255:
    jmp .L253
.L252:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1189]
    call w_emit_cn_fatal_cn_error_cb_
.L253:
    jmp .L251
.L250:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1217]
    call w_emit_cn_fatal_cn_error_cb_
.L251:
    ret
w_token_cn_args_cn_3:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L256
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_tuck
    call w_token_cn_next_cn_arg_cn_end
    call w_token_cn_type_dp_
    call w_TOKEN_fp_COMMA
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L258
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_tuck
    call w_token_cn_next_cn_arg_cn_end
    call w_token_cn_type_dp_
    call w_TOKEN_fp_COMMA
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L260
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_tuck
    call w_token_cn_next_cn_arg_cn_end
    call w_token_cn_type_dp_
    call w_TOKEN_fp_RPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L262
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_drop2
    jmp .L263
.L262:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1243]
    call w_emit_cn_fatal_cn_error_cb_
.L263:
    jmp .L261
.L260:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1280]
    call w_emit_cn_fatal_cn_error_cb_
.L261:
    jmp .L259
.L258:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1308]
    call w_emit_cn_fatal_cn_error_cb_
.L259:
    jmp .L257
.L256:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1336]
    call w_emit_cn_fatal_cn_error_cb_
.L257:
    ret
w_DEF_fp_WORD:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    ret
w_name_cn_sort_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_DEF_fp_SORT]
    call w_byte_cb__cb_
    ret
w_name_cn_value_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_DEF_fp_VALUE]
    call w_long_cb__cb_
    ret
w_DEF_fp_BUFFER:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 2
    ret
w_buffer_cn_alloc_cb_:
    call w_num_cn_buffers_ea_
    call w_MAX_fp_BUFFERS
    call w__do__dn_
    test rax, rax
    jz .L264
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1362]
    call w_panic_cb_
    jmp .L265
.L264:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_buffers_ea_
    call w_buffer_cn_size_cb_
    call w_heap_cn_alloc_cb_
    call w_num_cn_buffers_ea_
    call w_buffer_cn_base_cb_
    call w_num_cn_buffers_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_1_cl_
    call w_num_cn_buffers_cb_
.L265:
    ret
w_run_cn_output_cn_c99_cb_:
    call w_strings_cn_load_cb_
    call w_str_cn_buf_cn_create_cn_file_cb_
    call w_file_cn_out_cb_
    call w_c99_cn_emit_cn_header_cb_
    call w_c99_cn_emit_cn_strings_cb_
    call w_c99_cn_emit_cn_prims_cb_
    call w_c99_cn_emit_cn_buffers_cb_
    call w_c99_cn_emit_cn_word_cn_sigs_cb_
    call w_c99_cn_emit_cn_word_cn_defs_cb_
    call w_c99_cn_emit_cn_main_cb_
    call w_stdout
    call w_file_cn_out_cb_
    ret
w_token_cn_next_cn_arg_cn_end:
    call w_token_cn_is_cn_arg_cn_end_dp_
    call w_not
.L266:
    test rax, rax
    jz .L267
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_next
    call w_token_cn_is_cn_arg_cn_end_dp_
    call w_not
    jmp .L266
.L267:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_token_cn_is_cn_arg_cn_end_dp_:
    call w_token_cn_type_dp_
    call w_TOKEN_fp_COMMA
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L268
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    jmp .L269
.L268:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_RPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
.L269:
    ret
w_token_cn_next:
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L270
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_ea_
    call w_1_cl_
    jmp .L271
.L270:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L272
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
    call w_token_cn_type_dp_
    call w_TOKEN_fp_LPAREN
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L274
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_ea_
    call w_1_cl_
    jmp .L275
.L274:
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L275:
    jmp .L273
.L272:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_1_cl_
.L273:
.L271:
    ret
w_MAX_fp_BUFFERS:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1024
    ret
w_PAGE_fp_SIZE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 8192
    ret
w_num_cn_buffers_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NUM_fp_BUFFERS]
    mov rax, [rax]
    ret
w_num_cn_buffers_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_NUM_fp_BUFFERS]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_buffer_cn_size_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_BUFFER_fp_SIZE]
    call w_long_ea__ea_
    ret
w_buffer_cn_size_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_BUFFER_fp_SIZE]
    call w_long_cb__cb_
    ret
w_heap_cn_alloc_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 63
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 64
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_heap_cn_reserve_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_heap_cn_length_ea_
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w_heap_cn_length_cb_
    call w_heap_cn_base_ea_
    call w_tuck
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w_heap_cn_base_cb_
    ret
w_buffer_cn_base_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_BUFFER_fp_BASE]
    call w_long_cb__cb_
    ret
w_DEF_fp_NONE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    ret
w_name_cn_sort_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_DEF_fp_SORT]
    call w_byte_ea__ea_
    ret
w_name_cn_sort_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_sort_ea_
    ret
w_name_cn_value_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_value_ea_
    ret
w_name_cn_defined_dp_:
    call w_name_cn_undefined_dp_
    call w_not
    ret
w_c99_cn_emit_cn_header_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1430]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1472]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1493]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1513]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1536]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1555]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1574]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1600]
    call w__dl__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1622]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1646]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1677]
    call w__dl__dl_
    ret
w_c99_cn_emit_cn_strings_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1717]
    call w__co_
    call w_strings_cn_size_ea_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1739]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1740]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_strings_cn_size_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L276:
    test rax, rax
    jz .L277
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_STRINGS_fp_BUF]
    call w_byte_ea__ea_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1779]
    call w__co_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_strings_cn_size_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L276
.L277:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1782]
    call w__dl__dl_
    ret
w_c99_cn_emit_cn_prims_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1866]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1894]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1921]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1949]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1962]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1987]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2003]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2006]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2033]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2050]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2056]
    call w__dl__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2058]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2088]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2112]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2133]
    call w__dl__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2135]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2166]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2189]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2212]
    call w__dl__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2214]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2245]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2263]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2288]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2304]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2329]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2344]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2347]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2374]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2391]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2397]
    call w__dl__dl_
    call w_PRIM_fp_ID
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2399]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2402]
    call w__dl__dl_
    call w_PRIM_fp_DUP
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2404]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2407]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2432]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2454]
    call w__dl__dl_
    call w_PRIM_fp_DROP
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2456]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2459]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2470]
    call w__dl__dl_
    call w_PRIM_fp_SWAP
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2472]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2475]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2498]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2521]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2543]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_ADD
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2545]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2548]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2571]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2594]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2611]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_SUB
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2613]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2616]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2639]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2662]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2679]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_MUL
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2688]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2691]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2714]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2737]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2754]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_DIV
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2756]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2759]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2782]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2805]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2822]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_MOD
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2824]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2827]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2850]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2880]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2897]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_EQ
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2899]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2902]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2925]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2948]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2966]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_LT
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2968]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2971]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+2994]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3017]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3034]
    call w__dl__dl_
    call w_PRIM_fp_INT_fp_LE
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3036]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3039]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3062]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3085]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3103]
    call w__dl__dl_
    call w_PRIM_fp_POSIX_fp_WRITE
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3105]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3108]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3138]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3163]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3187]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3207]
    call w__dl__dl_
    call w_PRIM_fp_POSIX_fp_READ
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3209]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3212]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3242]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3267]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3291]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3314]
    call w__dl__dl_
    call w_PRIM_fp_POSIX_fp_OPEN
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3316]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3319]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3343]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3367]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3392]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3415]
    call w__dl__dl_
    call w_PRIM_fp_POSIX_fp_CLOSE
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3417]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3420]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3444]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3464]
    call w__dl__dl_
    call w_PRIM_fp_POSIX_fp_EXIT
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3466]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3469]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3493]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3506]
    call w__dl__dl_
    call w_PRIM_fp_POSIX_fp_MMAP
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3508]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3511]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3535]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3559]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3584]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3608]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3638]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3663]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3696]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3718]
    call w__dl__dl_
    call w_PRIM_fp_DEBUG
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3720]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3723]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3744]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3748]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3751]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3805]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3830]
    call w__co_q
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3840]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3860]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3866]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3889]
    call w__dl__dl_
    call w_PRIM_fp_MIRTH_fp_REVISION
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3891]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3894]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3907]
    call w__dl__dl_
    call w_PRIM_fp_MEM_fp_GET
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3909]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3912]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3952]
    call w__dl__dl_
    call w_PRIM_fp_MEM_fp_SET
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3954]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3957]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+3984]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4008]
    call w__dl__dl_
    call w_PRIM_fp_MEM_fp_GET_fp_BYTE
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4010]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4013]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4040]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4053]
    call w__dl__dl_
    call w_PRIM_fp_MEM_fp_SET_fp_BYTE
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4055]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4058]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4085]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4103]
    call w__dl__dl_
    ret
w_c99_cn_emit_cn_buffers_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L278:
    test rax, rax
    jz .L279
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_c99_cn_emit_cn_buffer_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L278
.L279:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1792]
    call w__dl_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_c99_cn_emit_cn_word_cn_sigs_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L280:
    test rax, rax
    jz .L281
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_c99_cn_emit_cn_word_cn_sig_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L280
.L281:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4105]
    call w__dl_
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_c99_cn_emit_cn_word_cn_defs_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
.L282:
    test rax, rax
    jz .L283
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_c99_cn_emit_cn_word_cn_def_cb_
    call w_1_cl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_num_cn_names_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    jmp .L282
.L283:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_c99_cn_emit_cn_main_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4337]
    call w__dl_
    call w_c99_cn_emit_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4355]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4369]
    call w__dl_
    ret
w__dl_:
    call w_str_cn_print_cn_ln_cb_
    ret
w__dl__dl_:
    call w_str_cn_print_cn_ln_cb_
    call w_print_cn_ln_cb_
    ret
w__co_q:
    call w_str_cn_print_cb_
    call w_print_cn_quote_cb_
    ret
w__co_:
    call w_str_cn_print_cb_
    ret
w__co_n:
    call w_int_cn_print_cb_
    ret
w__co_w:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1397]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_load_cb_
    call w_str_cn_buf_cn_print_cb_
    call w_print_cn_ln_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1408]
    call w__co_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1422]
    call w__co_
    ret
w_c99_cn_emit_cn_buffer_cb_:
    call w_name_cn_is_cn_buffer_dp_
    test rax, rax
    jz .L284
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1793]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1810]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_name_cn_value_ea_
    call w_buffer_cn_size_ea_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1812]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1821]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1835]
    call w__co_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+1861]
    call w__dl_
    jmp .L285
.L284:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L285:
    ret
w_c99_cn_emit_cn_word_cn_sig_cb_:
    call w_name_cn_is_cn_word_dp_
    test rax, rax
    jz .L286
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4106]
    call w__co_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4120]
    call w__dl_
    jmp .L287
.L286:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L287:
    ret
w_c99_cn_emit_cn_word_cn_def_cb_:
    call w_name_cn_is_cn_word_dp_
    test rax, rax
    jz .L288
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_w
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4129]
    call w__dl_
    call w_name_cn_value_ea_
    call w_c99_cn_emit_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4131]
    call w__dl__dl_
    jmp .L289
.L288:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L289:
    ret
w_c99_cn_emit_cn_run_cb_:
    call w_token_cn_run_cn_end_dp_
    call w_not
.L290:
    test rax, rax
    jz .L291
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_c99_cn_emit_cn_token_cb_
    call w_token_cn_next
    call w_token_cn_run_cn_end_dp_
    call w_not
    jmp .L290
.L291:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_c99_cn_emit_cn_token_cb_:
    call w_token_cn_type_dp_
    call w_TOKEN_fp_INT
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L292
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4133]
    call w__co_
    call w_token_cn_value_ea_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4143]
    call w__dl_
    jmp .L293
.L292:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_STR
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L294
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4146]
    call w__co_
    call w_token_cn_value_ea_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4176]
    call w__dl_
    jmp .L295
.L294:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_type_dp_
    call w_TOKEN_fp_NAME
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L296
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_c99_cn_emit_cn_word_cb_
    jmp .L297
.L296:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L297:
.L295:
.L293:
    ret
w_c99_cn_emit_cn_word_cb_:
    call w_token_cn_value_dp_
    call w_PRIM_fp_DIP
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L298
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_args_cn_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4180]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4194]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_c99_cn_emit_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4204]
    call w__co_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4215]
    call w__dl_
    jmp .L299
.L298:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_dp_
    call w_PRIM_fp_IF
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L300
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_args_cn_2
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4224]
    call w__dl_
    push rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_c99_cn_emit_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    pop rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4241]
    call w__dl_
    call w_c99_cn_emit_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4254]
    call w__dl_
    jmp .L301
.L300:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_dp_
    call w_PRIM_fp_WHILE
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L302
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_args_cn_1
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4260]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4279]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4291]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4295]
    call w__dl_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4307]
    call w__co_
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4318]
    call w__dl_
    call w_c99_cn_emit_cn_run_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4321]
    call w__dl_
    jmp .L303
.L302:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_token_cn_value_ea_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4327]
    call w__co_
    call w__co_n
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4333]
    call w__dl_
.L303:
.L301:
.L299:
    ret
w_BASE_fp_HEAP_fp_SIZE:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 65536
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 90
    imul rax, [rbx]
    lea rbx, [rbx+8]
    ret
w_heap_cn_length_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_HEAP_fp_LENGTH]
    mov rax, [rax]
    ret
w_heap_cn_length_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_HEAP_fp_LENGTH]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_heap_cn_base_ea_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_HEAP_fp_BASE]
    mov rax, [rax]
    ret
w_heap_cn_base_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel b_HEAP_fp_BASE]
    mov rcx, [rbx]
    mov [rax], rcx
    mov rax, [rbx+8]
    lea rbx, [rbx+16]
    ret
w_heap_cn_reserve_cb_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_heap_cn_length_ea_
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setge al
    movzx eax, al
    test rax, rax
    jz .L304
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L305
.L304:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_heap_cn_new_cn_size
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_heap_cn_base_ea_
    call w_heap_cn_length_ea_
    add rax, [rbx]
    lea rbx, [rbx+8]
    mov rcx, [rbx]
    mov [rbx], rax
    mov rax, rcx
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 3
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4098
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, -1
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    push rbx
    push rax
    push 0x0A3F3F
    mov rdi, 2
    mov rsi, rsp
    mov rdx, 3
    mov rax, 0x2000004
    syscall
    pop rax
    pop rax
    pop rbx
    mov rdi, [rbx+32]
    mov rsi, [rbx+24]
    mov rdx, [rbx+16]
    mov rcx, [rbx+8]
    mov r8, [rbx]
    mov r9, rax
    mov rax, 0x20000C5
    syscall
    lea rbx, [rbx+40]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w__do_
    test rax, rax
    jz .L306
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    call w_heap_cn_base_ea_
    call w_heap_cn_length_ea_
    add rax, [rbx]
    lea rbx, [rbx+8]
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    test rax, rax
    jz .L308
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_heap_cn_length_ea_
    add rax, [rbx]
    lea rbx, [rbx+8]
    call w_heap_cn_length_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4371]
    call w_str_cn_trace_cn_ln_cb_
    jmp .L309
.L308:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_heap_cn_base_cb_
    call w_heap_cn_length_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4390]
    call w_str_cn_trace_cn_ln_cb_
.L309:
    jmp .L307
.L306:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4416]
    call w_panic_cb_
.L307:
.L305:
    ret
w_heap_cn_new_cn_size:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4095
    add rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 4096
    mov rcx, rax
    mov rax, [rbx]
    lea rbx, [rbx+8]
    cqo
    idiv rcx
    mov rax, rdx
    sub rax, [rbx]
    neg rax
    lea rbx, [rbx+8]
    call w_BASE_fp_HEAP_fp_SIZE
    call w_max
    ret
w_max:
    call w_dup2
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    setg al
    movzx eax, al
    test rax, rax
    jz .L310
    mov rax, [rbx]
    lea rbx, [rbx+8]
    call w_nip
    jmp .L311
.L310:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    mov rax, [rbx]
    lea rbx, [rbx+8]
.L311:
    ret
w_main:
    call w_init_cb_
    call w_run_cn_tests
    call w_read_cn_mirth_cn_src_cb_
    call w_init_cn_names_cb_
    call w_run_cn_lexer_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4448]
    call w_str_cn_trace_cn_ln_cb_
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    call w_token_cn_run_cb_
    call w_vstack_cn_empty_dp_
    test rax, rax
    jz .L312
    mov rax, [rbx]
    lea rbx, [rbx+8]
    jmp .L313
.L312:
    mov rax, [rbx]
    lea rbx, [rbx+8]
    lea rbx, [rbx-8]
    mov [rbx], rax
    lea rax, [rel strings+4458]
    call w_str_cn_trace_cb_
    call w_vstack_cn_trace_cb_
.L313:
    ret
w_in_cn_bootstrap_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
w_in_cn_mirth_cn_proper_dp_:
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 0
    lea rbx, [rbx-8]
    mov [rbx], rax
    mov rax, 1
    cmp rax, [rbx]
    lea rbx, [rbx+8]
    sete al
    movzx eax, al
    ret
section .data
    strings: db 041h, 073h, 073h, 065h, 072h, 074h, 069h, 06Fh, 06Eh, 020h, 066h, 061h, 069h, 06Ch, 065h, 064h, 000h, 073h, 074h, 072h, 02Dh, 062h, 075h, 066h, 02Dh, 070h, 075h, 073h, 068h, 021h, 020h, 062h, 075h, 074h, 020h, 053h, 054h, 052h, 05Fh, 042h, 055h, 046h, 020h, 069h, 073h, 020h, 061h, 06Ch, 072h, 065h, 061h, 064h, 079h, 020h, 066h, 075h, 06Ch, 06Ch, 000h, 000h, 000h, 000h, 000h, 000h, 073h, 074h, 072h, 02Dh, 062h, 075h, 066h, 02Dh, 072h, 065h, 061h, 064h, 021h, 020h, 066h, 061h, 069h, 06Ch, 065h, 064h, 000h, 041h, 061h, 042h, 062h, 000h, 000h, 068h, 065h, 06Ch, 06Ch, 06Fh, 000h, 068h, 065h, 06Ch, 06Ch, 06Fh, 02Ch, 020h, 077h, 06Fh, 072h, 06Ch, 064h, 021h, 000h, 030h, 000h, 070h, 061h, 06Eh, 069h, 063h, 03Ah, 020h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 066h, 061h, 069h, 06Ch, 065h, 064h, 020h, 074h, 06Fh, 020h, 06Fh, 070h, 065h, 06Eh, 020h, 066h, 069h, 06Ch, 065h, 021h, 000h, 066h, 061h, 069h, 06Ch, 065h, 064h, 020h, 074h, 06Fh, 020h, 06Fh, 070h, 065h, 06Eh, 020h, 066h, 069h, 06Ch, 065h, 021h, 000h, 066h, 061h, 069h, 06Ch, 065h, 064h, 020h, 074h, 06Fh, 020h, 072h, 065h, 061h, 064h, 020h, 066h, 069h, 06Ch, 065h, 000h, 000h, 000h, 066h, 061h, 069h, 06Ch, 065h, 064h, 020h, 074h, 06Fh, 020h, 063h, 06Ch, 06Fh, 073h, 065h, 020h, 066h, 069h, 06Ch, 065h, 000h, 052h, 065h, 061h, 064h, 069h, 06Eh, 067h, 020h, 06Dh, 069h, 072h, 074h, 068h, 02Eh, 06Dh, 074h, 068h, 000h, 06Dh, 069h, 072h, 074h, 068h, 02Eh, 06Dh, 074h, 068h, 000h, 052h, 065h, 061h, 064h, 020h, 000h, 020h, 062h, 079h, 074h, 065h, 073h, 020h, 028h, 046h, 049h, 04Ch, 045h, 05Fh, 042h, 055h, 046h, 05Fh, 053h, 049h, 05Ah, 045h, 020h, 03Dh, 020h, 000h, 029h, 02Eh, 000h, 03Ah, 020h, 000h, 065h, 06Eh, 064h, 000h, 069h, 064h, 000h, 064h, 075h, 070h, 000h, 064h, 072h, 06Fh, 070h, 000h, 073h, 077h, 061h, 070h, 000h, 064h, 069h, 070h, 000h, 069h, 066h, 000h, 077h, 068h, 069h, 06Ch, 065h, 000h, 02Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 02Dh, 000h, 02Ah, 000h, 02Fh, 000h, 025h, 000h, 03Dh, 000h, 03Ch, 000h, 03Ch, 03Dh, 000h, 040h, 000h, 021h, 000h, 062h, 079h, 074h, 065h, 040h, 000h, 062h, 079h, 074h, 065h, 021h, 000h, 070h, 06Fh, 073h, 069h, 078h, 02Dh, 072h, 065h, 061h, 064h, 021h, 000h, 070h, 06Fh, 073h, 069h, 078h, 02Dh, 077h, 072h, 069h, 074h, 065h, 021h, 000h, 070h, 06Fh, 073h, 069h, 078h, 02Dh, 06Fh, 070h, 065h, 06Eh, 021h, 000h, 070h, 06Fh, 073h, 069h, 078h, 02Dh, 063h, 06Ch, 06Fh, 073h, 065h, 021h, 000h, 070h, 06Fh, 073h, 069h, 078h, 02Dh, 065h, 078h, 069h, 074h, 021h, 000h, 070h, 06Fh, 073h, 069h, 078h, 02Dh, 06Dh, 06Dh, 061h, 070h, 021h, 000h, 03Fh, 03Fh, 000h, 04Dh, 049h, 052h, 054h, 048h, 05Fh, 052h, 045h, 056h, 049h, 053h, 049h, 04Fh, 04Eh, 000h, 000h, 000h, 000h, 000h, 000h, 064h, 065h, 066h, 000h, 064h, 065h, 066h, 02Dh, 073h, 074h, 061h, 074h, 069h, 063h, 02Dh, 062h, 075h, 066h, 066h, 065h, 072h, 000h, 06Fh, 075h, 074h, 070h, 075h, 074h, 02Dh, 061h, 073h, 06Dh, 000h, 06Fh, 075h, 074h, 070h, 075h, 074h, 02Dh, 063h, 039h, 039h, 000h, 04Eh, 04Fh, 04Eh, 045h, 000h, 04Ch, 050h, 041h, 052h, 045h, 04Eh, 000h, 052h, 050h, 041h, 052h, 045h, 04Eh, 000h, 000h, 043h, 04Fh, 04Dh, 04Dh, 041h, 000h, 04Eh, 041h, 04Dh, 045h, 000h, 049h, 04Eh, 054h, 000h, 053h, 054h, 052h, 000h, 03Fh, 03Fh, 03Fh, 055h, 04Eh, 04Bh, 04Eh, 04Fh, 057h, 04Eh, 03Fh, 03Fh, 03Fh, 000h, 03Ah, 000h, 03Ah, 000h, 03Ah, 000h, 03Ah, 000h, 063h, 06Fh, 06Dh, 070h, 069h, 06Ch, 065h, 072h, 020h, 065h, 072h, 072h, 06Fh, 072h, 03Ah, 020h, 06Ch, 065h, 078h, 065h, 072h, 020h, 073h, 074h, 061h, 063h, 06Bh, 020h, 06Fh, 076h, 065h, 072h, 066h, 06Ch, 06Fh, 077h, 000h, 063h, 06Fh, 06Dh, 070h, 069h, 06Ch, 065h, 072h, 020h, 065h, 072h, 072h, 06Fh, 072h, 03Ah, 020h, 06Ch, 065h, 078h, 065h, 072h, 020h, 073h, 074h, 061h, 063h, 06Bh, 020h, 075h, 06Eh, 064h, 065h, 072h, 066h, 06Ch, 06Fh, 077h, 000h, 073h, 074h, 061h, 063h, 06Bh, 020h, 075h, 06Eh, 064h, 065h, 072h, 066h, 06Ch, 06Fh, 077h, 03Ah, 020h, 061h, 074h, 074h, 065h, 06Dh, 070h, 074h, 065h, 064h, 020h, 074h, 06Fh, 020h, 070h, 06Fh, 070h, 020h, 06Fh, 06Eh, 020h, 065h, 06Dh, 070h, 074h, 079h, 020h, 076h, 073h, 074h, 061h, 063h, 06Bh, 000h, 073h, 074h, 061h, 063h, 06Bh, 020h, 06Fh, 076h, 065h, 072h, 066h, 06Ch, 06Fh, 077h, 03Ah, 020h, 061h, 074h, 074h, 065h, 06Dh, 070h, 074h, 065h, 064h, 020h, 074h, 06Fh, 020h, 070h, 075h, 073h, 068h, 020h, 06Fh, 06Eh, 020h, 066h, 075h, 06Ch, 06Ch, 020h, 076h, 073h, 074h, 061h, 063h, 06Bh, 000h, 073h, 074h, 061h, 063h, 06Bh, 020h, 075h, 06Eh, 064h, 065h, 072h, 066h, 06Ch, 06Fh, 077h, 03Ah, 020h, 061h, 074h, 074h, 065h, 06Dh, 070h, 074h, 065h, 064h, 020h, 074h, 06Fh, 020h, 072h, 065h, 061h, 064h, 020h, 066h, 072h, 06Fh, 06Dh, 020h, 074h, 06Fh, 070h, 020h, 06Fh, 066h, 020h, 065h, 06Dh, 070h, 074h, 079h, 020h, 076h, 073h, 074h, 061h, 063h, 06Bh, 000h, 073h, 074h, 061h, 063h, 06Bh, 020h, 075h, 06Eh, 064h, 065h, 072h, 066h, 06Ch, 06Fh, 077h, 03Ah, 020h, 061h, 074h, 074h, 065h, 06Dh, 070h, 074h, 065h, 064h, 020h, 074h, 06Fh, 020h, 077h, 072h, 069h, 074h, 065h, 020h, 074h, 06Fh, 020h, 074h, 06Fh, 070h, 020h, 06Fh, 066h, 020h, 065h, 06Dh, 070h, 074h, 079h, 020h, 076h, 073h, 074h, 061h, 063h, 06Bh, 000h, 06Dh, 069h, 072h, 074h, 068h, 02Eh, 06Dh, 074h, 068h, 03Ah, 000h, 077h, 061h, 072h, 06Eh, 069h, 06Eh, 067h, 03Ah, 020h, 000h, 06Dh, 069h, 072h, 074h, 068h, 02Eh, 06Dh, 074h, 068h, 03Ah, 000h, 065h, 072h, 072h, 06Fh, 072h, 03Ah, 020h, 000h, 055h, 06Eh, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 074h, 06Fh, 06Bh, 065h, 06Eh, 020h, 074h, 079h, 070h, 065h, 020h, 069h, 06Eh, 020h, 074h, 06Fh, 06Bh, 065h, 06Eh, 02Dh, 072h, 075h, 06Eh, 021h, 000h, 057h, 06Fh, 072h, 064h, 020h, 069h, 073h, 020h, 075h, 06Eh, 064h, 065h, 066h, 069h, 06Eh, 065h, 064h, 02Eh, 000h, 044h, 06Fh, 06Eh, 027h, 074h, 020h, 06Bh, 06Eh, 06Fh, 077h, 020h, 068h, 06Fh, 077h, 020h, 074h, 06Fh, 020h, 072h, 075h, 06Eh, 020h, 077h, 06Fh, 072h, 064h, 02Eh, 000h, 03Fh, 03Fh, 020h, 000h, 077h, 06Fh, 072h, 064h, 020h, 061h, 06Ch, 072h, 065h, 061h, 064h, 079h, 020h, 064h, 065h, 066h, 069h, 06Eh, 065h, 064h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 077h, 06Fh, 072h, 064h, 020h, 06Eh, 061h, 06Dh, 065h, 000h, 062h, 075h, 066h, 066h, 065h, 072h, 020h, 061h, 06Ch, 072h, 065h, 061h, 064h, 079h, 020h, 064h, 065h, 066h, 069h, 06Eh, 065h, 064h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 062h, 075h, 066h, 066h, 065h, 072h, 020h, 06Eh, 061h, 06Dh, 065h, 000h, 044h, 06Fh, 06Eh, 074h, 020h, 06Bh, 06Eh, 06Fh, 077h, 020h, 068h, 06Fh, 077h, 020h, 074h, 06Fh, 020h, 072h, 075h, 06Eh, 020h, 070h, 072h, 069h, 06Dh, 020h, 079h, 065h, 074h, 02Eh, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 06Eh, 06Fh, 020h, 061h, 072h, 067h, 073h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 031h, 020h, 061h, 072h, 067h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 074h, 06Fh, 06Fh, 020h, 06Dh, 061h, 06Eh, 079h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 031h, 020h, 061h, 072h, 067h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 06Eh, 06Fh, 06Eh, 065h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 032h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 074h, 06Fh, 06Fh, 020h, 06Dh, 061h, 06Eh, 079h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 032h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 06Fh, 06Eh, 06Ch, 079h, 020h, 031h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 032h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 06Eh, 06Fh, 06Eh, 065h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 033h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 074h, 06Fh, 06Fh, 020h, 06Dh, 061h, 06Eh, 079h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 033h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 06Fh, 06Eh, 06Ch, 079h, 020h, 032h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 033h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 06Fh, 06Eh, 06Ch, 079h, 020h, 031h, 000h, 065h, 078h, 070h, 065h, 063h, 074h, 065h, 064h, 020h, 033h, 020h, 061h, 072h, 067h, 073h, 02Ch, 020h, 067h, 06Fh, 074h, 020h, 06Eh, 06Fh, 06Eh, 065h, 000h, 063h, 06Fh, 06Dh, 070h, 069h, 06Ch, 065h, 072h, 020h, 065h, 072h, 072h, 06Fh, 072h, 03Ah, 020h, 072h, 061h, 06Eh, 020h, 06Fh, 075h, 074h, 020h, 06Fh, 066h, 020h, 062h, 075h, 066h, 066h, 065h, 072h, 073h, 000h, 02Fh, 02Fh, 020h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 076h, 06Fh, 069h, 064h, 020h, 077h, 000h, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 000h, 02Fh, 02Ah, 020h, 043h, 039h, 039h, 020h, 061h, 075h, 074h, 06Fh, 067h, 065h, 06Eh, 065h, 072h, 061h, 074h, 065h, 064h, 020h, 062h, 079h, 020h, 06Dh, 069h, 072h, 074h, 068h, 020h, 063h, 06Fh, 06Dh, 070h, 069h, 06Ch, 065h, 072h, 020h, 02Ah, 02Fh, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 073h, 074h, 064h, 062h, 06Fh, 06Fh, 06Ch, 02Eh, 068h, 03Eh, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 073h, 074h, 064h, 069h, 06Eh, 074h, 02Eh, 068h, 03Eh, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 073h, 074h, 064h, 06Ch, 069h, 062h, 02Eh, 068h, 03Eh, 000h, 000h, 000h, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 073h, 074h, 064h, 069h, 06Fh, 02Eh, 068h, 03Eh, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 066h, 063h, 06Eh, 074h, 06Ch, 02Eh, 068h, 03Eh, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 075h, 06Eh, 069h, 073h, 074h, 064h, 02Eh, 068h, 03Eh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 023h, 069h, 06Eh, 063h, 06Ch, 075h, 064h, 065h, 020h, 03Ch, 073h, 079h, 073h, 02Fh, 06Dh, 06Dh, 061h, 06Eh, 02Eh, 068h, 03Eh, 000h, 023h, 064h, 065h, 066h, 069h, 06Eh, 065h, 020h, 053h, 054h, 041h, 043h, 04Bh, 05Fh, 053h, 049h, 05Ah, 045h, 020h, 032h, 030h, 030h, 030h, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 020h, 073h, 063h, 020h, 03Dh, 020h, 053h, 054h, 041h, 043h, 04Bh, 05Fh, 053h, 049h, 05Ah, 045h, 03Bh, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 073h, 074h, 061h, 063h, 06Bh, 05Bh, 053h, 054h, 041h, 043h, 04Bh, 05Fh, 053h, 049h, 05Ah, 045h, 05Dh, 020h, 03Dh, 020h, 07Bh, 030h, 07Dh, 03Bh, 000h, 023h, 064h, 065h, 066h, 069h, 06Eh, 065h, 020h, 053h, 054h, 052h, 049h, 04Eh, 047h, 053h, 05Fh, 053h, 049h, 05Ah, 045h, 020h, 000h, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 063h, 068h, 061h, 072h, 020h, 073h, 074h, 072h, 069h, 06Eh, 067h, 073h, 05Bh, 053h, 054h, 052h, 049h, 04Eh, 047h, 053h, 05Fh, 053h, 049h, 05Ah, 045h, 05Dh, 020h, 03Dh, 020h, 07Bh, 020h, 000h, 02Ch, 020h, 000h, 07Dh, 03Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 075h, 069h, 06Eh, 074h, 038h, 05Fh, 074h, 020h, 062h, 000h, 05Bh, 000h, 05Dh, 020h, 03Dh, 020h, 07Bh, 030h, 07Dh, 03Bh, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 076h, 06Fh, 069h, 064h, 020h, 077h, 000h, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 020h, 07Bh, 020h, 070h, 075h, 073h, 068h, 028h, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 029h, 062h, 000h, 029h, 03Bh, 020h, 07Dh, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 070h, 06Fh, 070h, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 066h, 020h, 028h, 073h, 063h, 020h, 03Ch, 020h, 053h, 054h, 041h, 043h, 04Bh, 05Fh, 053h, 049h, 05Ah, 045h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 072h, 065h, 074h, 075h, 072h, 06Eh, 020h, 073h, 074h, 061h, 063h, 06Bh, 05Bh, 073h, 063h, 02Bh, 02Bh, 05Dh, 03Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 020h, 065h, 06Ch, 073h, 065h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 066h, 070h, 072h, 069h, 06Eh, 074h, 066h, 028h, 073h, 074h, 064h, 065h, 072h, 072h, 02Ch, 020h, 000h, 053h, 054h, 041h, 043h, 04Bh, 020h, 055h, 04Eh, 044h, 045h, 052h, 046h, 04Ch, 04Fh, 057h, 000h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 066h, 070h, 075h, 074h, 063h, 028h, 031h, 030h, 02Ch, 020h, 073h, 074h, 064h, 065h, 072h, 072h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 065h, 078h, 069h, 074h, 028h, 031h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 000h, 07Dh, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 076h, 06Fh, 069h, 064h, 02Ah, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 000h, 020h, 020h, 020h, 020h, 072h, 065h, 074h, 075h, 072h, 06Eh, 020h, 028h, 076h, 06Fh, 069h, 064h, 02Ah, 029h, 078h, 03Bh, 000h, 07Dh, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 075h, 069h, 06Eh, 074h, 038h, 05Fh, 074h, 020h, 070h, 06Fh, 070h, 05Fh, 075h, 038h, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 072h, 065h, 074h, 075h, 072h, 06Eh, 020h, 028h, 075h, 069h, 06Eh, 074h, 038h, 05Fh, 074h, 029h, 078h, 03Bh, 000h, 07Dh, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 076h, 06Fh, 069h, 064h, 020h, 070h, 075h, 073h, 068h, 020h, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 066h, 020h, 028h, 073h, 063h, 020h, 03Eh, 020h, 030h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 073h, 074h, 061h, 063h, 06Bh, 05Bh, 02Dh, 02Dh, 073h, 063h, 05Dh, 020h, 03Dh, 020h, 078h, 03Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 020h, 065h, 06Ch, 073h, 065h, 020h, 07Bh, 000h, 000h, 000h, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 066h, 070h, 072h, 069h, 06Eh, 074h, 066h, 028h, 073h, 074h, 064h, 065h, 072h, 072h, 02Ch, 020h, 000h, 053h, 054h, 041h, 043h, 04Bh, 020h, 04Fh, 056h, 045h, 052h, 046h, 04Ch, 04Fh, 057h, 000h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 066h, 070h, 075h, 074h, 063h, 028h, 031h, 030h, 02Ch, 020h, 073h, 074h, 064h, 065h, 072h, 072h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 065h, 078h, 069h, 074h, 028h, 031h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 000h, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 078h, 029h, 03Bh, 020h, 070h, 075h, 073h, 068h, 028h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 078h, 029h, 03Bh, 020h, 070h, 075h, 073h, 068h, 028h, 079h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 078h, 020h, 02Bh, 020h, 079h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 079h, 020h, 02Dh, 020h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 078h, 020h, 02Ah, 020h, 079h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 079h, 020h, 02Fh, 020h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 079h, 020h, 025h, 020h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 078h, 020h, 03Dh, 03Dh, 020h, 079h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 079h, 020h, 03Ch, 020h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 079h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 079h, 020h, 03Ch, 03Dh, 020h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 020h, 06Eh, 020h, 03Dh, 020h, 028h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 076h, 06Fh, 069h, 064h, 02Ah, 020h, 070h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 066h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 077h, 072h, 069h, 074h, 065h, 028h, 066h, 02Ch, 020h, 070h, 02Ch, 020h, 06Eh, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 020h, 06Eh, 020h, 03Dh, 020h, 028h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 076h, 06Fh, 069h, 064h, 02Ah, 020h, 070h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 066h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 072h, 065h, 061h, 064h, 028h, 066h, 02Ch, 070h, 02Ch, 06Eh, 029h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 06Dh, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 066h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 076h, 06Fh, 069h, 064h, 02Ah, 020h, 070h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 06Fh, 070h, 065h, 06Eh, 028h, 070h, 02Ch, 066h, 02Ch, 06Dh, 029h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 078h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 063h, 06Ch, 06Fh, 073h, 065h, 028h, 078h, 029h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 078h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 065h, 078h, 069h, 074h, 028h, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 066h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 065h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 064h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 020h, 063h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 020h, 062h, 020h, 03Dh, 020h, 028h, 073h, 069h, 07Ah, 065h, 05Fh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 076h, 06Fh, 069h, 064h, 02Ah, 020h, 061h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 076h, 06Fh, 069h, 064h, 02Ah, 020h, 070h, 020h, 03Dh, 020h, 06Dh, 06Dh, 061h, 070h, 028h, 061h, 02Ch, 062h, 02Ch, 063h, 02Ch, 064h, 02Ch, 065h, 02Ch, 066h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 029h, 070h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 066h, 070h, 072h, 069h, 06Eh, 074h, 066h, 028h, 073h, 074h, 064h, 065h, 072h, 072h, 02Ch, 020h, 000h, 03Fh, 03Fh, 020h, 000h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 066h, 06Fh, 072h, 020h, 028h, 06Ch, 06Fh, 06Eh, 067h, 020h, 069h, 020h, 03Dh, 020h, 053h, 054h, 041h, 043h, 04Bh, 05Fh, 053h, 049h, 05Ah, 045h, 02Dh, 031h, 03Bh, 020h, 069h, 020h, 03Eh, 03Dh, 020h, 028h, 06Ch, 06Fh, 06Eh, 067h, 029h, 073h, 063h, 03Bh, 020h, 069h, 02Dh, 02Dh, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 020h, 066h, 070h, 072h, 069h, 06Eh, 074h, 066h, 028h, 073h, 074h, 064h, 065h, 072h, 072h, 02Ch, 020h, 000h, 025h, 070h, 020h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 02Ch, 020h, 028h, 076h, 06Fh, 069h, 064h, 02Ah, 029h, 073h, 074h, 061h, 063h, 06Bh, 05Bh, 069h, 05Dh, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 000h, 020h, 020h, 020h, 020h, 066h, 070h, 075h, 074h, 063h, 028h, 031h, 030h, 02Ch, 020h, 073h, 074h, 064h, 065h, 072h, 072h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 031h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 073h, 074h, 061h, 063h, 06Bh, 05Bh, 073h, 063h, 05Dh, 020h, 03Dh, 020h, 02Ah, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 02Ah, 029h, 028h, 073h, 074h, 061h, 063h, 06Bh, 05Bh, 073h, 063h, 05Dh, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 02Ah, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 02Ah, 078h, 020h, 03Dh, 020h, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 029h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 075h, 069h, 06Eh, 074h, 038h, 05Fh, 074h, 02Ah, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 02Ah, 078h, 029h, 03Bh, 000h, 07Dh, 000h, 020h, 07Bh, 000h, 020h, 020h, 020h, 075h, 069h, 06Eh, 074h, 038h, 05Fh, 074h, 02Ah, 020h, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 070h, 074h, 072h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 02Ah, 078h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 05Fh, 075h, 038h, 028h, 029h, 03Bh, 000h, 07Dh, 000h, 000h, 073h, 074h, 061h, 074h, 069h, 063h, 020h, 076h, 06Fh, 069h, 064h, 020h, 077h, 000h, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 03Bh, 000h, 07Bh, 000h, 07Dh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 000h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 029h, 028h, 073h, 074h, 072h, 069h, 06Eh, 067h, 073h, 020h, 02Bh, 020h, 000h, 029h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 064h, 000h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 064h, 000h, 029h, 03Bh, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 020h, 020h, 020h, 020h, 069h, 066h, 020h, 028h, 070h, 06Fh, 070h, 028h, 029h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 020h, 065h, 06Ch, 073h, 065h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 000h, 020h, 020h, 020h, 020h, 066h, 06Fh, 072h, 020h, 028h, 069h, 06Eh, 074h, 036h, 034h, 05Fh, 074h, 020h, 063h, 000h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 03Bh, 020h, 063h, 000h, 03Bh, 020h, 063h, 000h, 020h, 03Dh, 020h, 070h, 06Fh, 070h, 028h, 029h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 070h, 075h, 073h, 068h, 028h, 063h, 000h, 029h, 03Bh, 000h, 020h, 020h, 020h, 020h, 07Dh, 000h, 020h, 020h, 020h, 020h, 077h, 000h, 028h, 029h, 03Bh, 000h, 069h, 06Eh, 074h, 020h, 06Dh, 061h, 069h, 06Eh, 020h, 028h, 076h, 06Fh, 069h, 064h, 029h, 020h, 07Bh, 000h, 020h, 020h, 020h, 020h, 072h, 065h, 074h, 075h, 072h, 06Eh, 020h, 030h, 03Bh, 000h, 07Dh, 000h, 072h, 065h, 073h, 065h, 072h, 076h, 065h, 064h, 020h, 06Dh, 06Fh, 072h, 065h, 020h, 073h, 069h, 07Ah, 065h, 000h, 061h, 06Ch, 06Ch, 06Fh, 063h, 061h, 074h, 065h, 064h, 020h, 06Eh, 065h, 077h, 020h, 068h, 065h, 061h, 070h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 000h, 046h, 061h, 069h, 06Ch, 065h, 064h, 020h, 074h, 06Fh, 020h, 061h, 06Ch, 06Ch, 06Fh, 063h, 061h, 074h, 065h, 020h, 068h, 065h, 061h, 070h, 020h, 062h, 075h, 066h, 066h, 065h, 072h, 02Eh, 000h, 042h, 075h, 069h, 06Ch, 064h, 069h, 06Eh, 067h, 02Eh, 000h, 056h, 053h, 054h, 041h, 043h, 04Bh, 020h, 03Dh, 020h, 000h, 062h, 075h, 069h, 06Ch, 064h, 020h, 023h, 000h, 000h, 000h, 000h, 000h, 06Dh, 069h, 072h, 074h, 068h, 02Eh, 061h, 073h, 06Dh, 000h, 06Dh, 069h, 072h, 074h, 068h, 02Eh, 063h, 000h, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
section .bss
    b_STR_fp_BUF_fp_LEN: resb 8
    b_STR_fp_BUF: resb 4096
    b_TEST_fp_BUF: resb 16
    b_FILE_fp_IN: resb 8
    b_FILE_fp_OUT: resb 8
    b_FILE_fp_ERR: resb 8
    b_FILE_fp_BUF_fp_LENGTH: resb 8
    b_FILE_fp_BUF: resb 131072
    b_NUM_fp_NAMES: resb 8
    b_NAME_fp_BUF: resb 524288
    b_STRINGS_fp_SIZE: resb 8
    b_STRINGS_fp_BUF: resb 32768
    b_NUM_fp_TOKENS: resb 8
    b_TOKEN_fp_TYPE: resb 32768
    b_TOKEN_fp_VALUE: resb 262144
    b_TOKEN_fp_ROW: resb 262144
    b_TOKEN_fp_COL: resb 262144
    b_LEXER_fp_ROW: resb 8
    b_LEXER_fp_COL: resb 8
    b_LEXER_fp_IDX: resb 8
    b_LEXER_fp_STACK_fp_LENGTH: resb 8
    b_LEXER_fp_STACK_fp_BUF: resb 2048
    b_VSTACK_fp_LEN: resb 8
    b_VSTACK_fp_BUF: resb 65536
    b_NUM_fp_BUFFERS: resb 8
    b_BUFFER_fp_SIZE: resb 8192
    b_BUFFER_fp_BASE: resb 8192
    b_DEF_fp_SORT: resb 8192
    b_DEF_fp_VALUE: resb 65536
    b_HEAP_fp_LENGTH: resb 8
    b_HEAP_fp_BASE: resb 8
    vs: resq 0x10020
