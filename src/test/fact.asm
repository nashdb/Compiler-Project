# Generated by mc: 19:41 4.28.2017.
#
	.data
	.text
fact:	addi	$sp, $sp, -4		# push fp
	sw	$fp, 0($sp)
	move	$fp, $sp		# fp <- sp
	addi	$sp, $sp, -44		# allocate locals
	li	$v0, 0
	sw	$v0, -40($fp)
	lw	$t1, 8($fp)
	lw	$t2, -40($fp)
	move	$v0, $t1, $t2		# ==
	li	$v0, 1
	movz	$v0, $t1, $t2
	sw	$v0, -36($fp)
	lw	$v0, -36($fp)
	sw	$v0, -32($fp)
	lw	$v0, -32($fp)
	beqz	$v0, l0
	li	$v0, 1
	move	$sp, $fp
	jr	$ra				# return
	b	l1
l0:	nop
	li	$v0, 1
	sw	$v0, -28($fp)
	lw	$t1, 8($fp)
	lw	$t2, -28($fp)
	move	$v0, $t1, $t2
	sw	$v0, -24($fp)
	lw	$v0, -24($fp)
	sw	$v0, -20($fp)
	lw	$t1, -20($fp)
	addi	$sp, $sp, -4		# push 
	sw	$t1, 0($sp)
	addi	$sp, $sp, -4		# push ra
	sw	$ra, 0($sp)
	jal	fact				# funcall
	lw	$ra, 0($sp)		# pop ra
	addi	$sp, $sp, 4
	addi	$sp, $sp, 4		# deallocate args
	sw	$v0, -16($fp)
	lw	$v0, -16($fp)
	sw	$v0, -12($fp)
	lw	$t1, 8($fp)
	lw	$t2, -12($fp)
	mul	$v0, $t1, $t2
	sw	$v0, -8($fp)
	lw	$v0, -8($fp)
	sw	$v0, -4($fp)
	lw	$v0, -4($fp)
	move	$sp, $fp
	jr	$ra				# return
l1:	nop
