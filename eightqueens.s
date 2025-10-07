        # code
        .text
        .globl  main
main:   

        # allocate space for array a from the stack
        # the numbers in the array are the location of the queen in each row.
        # we can place the queen in row 0 in a different column
        # by changing the first 0 below.

        addi    sp, sp, -32	    # this represents space allocated for the chess board
        addi    a0, sp, 0           # put a's address in a0

        addi    t0, x0, 3           # place a queen in column 0 in row 0
        sw      t0, 0(a0)	    # queen is set to 0th position, i.e. arrayy a[0] = 0

        addi    a1, x0, 1           # number of queens already placed; now next row to fill is row 1
        jal	ra, solve_8queens   # recursively call this fucntion

        beq     x0, x0, exit

# print a string stored in a word array, and a newline
my_puts:
        addi    a7, x0, 11      # system call printing a character 
        addi    t0, a0, 0       # copy a0 to t0, which is the start address of the array
mp_loop:
        lw      a0, 0(t0)	# load a word from memory
        beq     a0, x0, mp_exit # if its 0, stop
        ecall			# else print it using ecall
        addi    t0, t0, 4	# move to the next character
        beq     x0, x0, mp_loop # loop- do same
mp_exit:
        # print newline
        addi    a0, x0, '\n'
        ecall
        jalr    x0, ra, 0	# function call, ra: return address

#######################
### put your code here

# difference(i, j) - Calculate absolute difference |i - j|
# Input: a0 = i, a1 = j
# Output: a0 = |i - j|
difference:
    sub     t0, a0, a1          # t0 = i - j
    srai    t1, t0, 31          # t1 = sign bit (all 1s if negative, 0 if positive)
    beq     t1, x0, diff_positive  # if sign bit is 0, t0 is already positive
    sub     t0, x0, t0          # negate t0 to make it positive
diff_positive:
    addi    a0, t0, 0           # return result in a0
    jalr    x0, ra, 0

# is_valid(a, row, column) - Check if placing queen at (row, column) is safe
# Input: a0 = array a, a1 = row, a2 = column
# Output: a0 = 1 if valid, 0 if invalid
is_valid:
    addi    sp, sp, -20		# allocating s;ace
    sw      ra, 16(sp)		# return address
    sw      s0, 12(sp)          # s0 = array a
    sw      s1, 8(sp)           # s1 = row
    sw      s2, 4(sp)           # s2 = column
    sw      s3, 0(sp)           # s3 = i (loop counter)
    # now saving registers into the stack because every loop/function call will change the value of a0
    addi    s0, a0, 0		#a0 = | i - j | , stored in s0 which is array a
    addi    s1, a1, 0		#a1 is j, s1 stores j
    addi    s2, a2, 0		#s2 stores column index
    addi    s3, x0, 0           # i = 0 which is loop countter = 0

iv_loop:    			# is valid loop
    # Check if i < row using subtraction and sign bit, if prev checked loop = or > than current row, all rows have been checked
    sub     t0, s3, s1          # t0 = i - row
    srai    t1, t0, 31          # t1 = sign bit of t0
    beq     t1, x0, iv_valid    # if i >= row, i.e. signed bit is 0, positive, we're done checking, position is valid, but if negative, we're still looping
    
    # Load a[i] -> old queen column
    slli    t2, s3, 2           # t2 = i * 4 (byte offset)
    add     t3, s0, t2          # t3 = address of a[i]
    lw      t4, 0(t3)           # t4 = a[i]
    
    # Check column threat: if (column == a[i])
    beq     s2, t4, iv_invalid  # if current queen coluymn = old queen's column --> invalid
    
    # Check diagonal threat: if (|i - row| == |a[i] - column|)
    # Calculate |i - row|
    addi    a0, s3, 0           # a0 = i
    addi    a1, s1, 0           # a1 = row
    jal     ra, difference	# function call to the difference function
    addi    t5, a0, 0           # t5 = |i - row|
    
    # Calculate |a[i] - column|
    addi    a0, t4, 0           # a0 = a[i]
    addi    a1, s2, 0           # a1 = column
    jal     ra, difference	# function call to the difference function
    addi    t6, a0, 0           # t6 = |a[i] - column|
    # --------------------------------------------------------------------
    # Compare the two differences
    beq     t5, t6, iv_invalid	# row 2 - col 1 = 1, row 1 - col 2 = 1, so if this diff is the same, conflicting positions for queen
    
    addi    s3, s3, 1           # i++
    beq     x0, x0, iv_loop	# always branch out to iv_return

iv_valid:
    addi    a0, x0, 1           # return 1 (valid)
    beq     x0, x0, iv_return   # always branch out to iv_return

iv_invalid:
    addi    a0, x0, 0           # return 0 (invalid)

iv_return:
    lw      ra, 16(sp)		
    lw      s0, 12(sp)
    lw      s1, 8(sp)		# we're saving all the saved values back into registers and emptyng the stack
    lw      s2, 4(sp)
    lw      s3, 0(sp)		
    addi    sp, sp, 20		# restore the stack pointer positions
    jalr    x0, ra, 0		# function return

# process_solution(a, k) - Print the board and solution
# Input: a0 = array a, a1 = k (should be 8)
process_solution:
    addi    sp, sp, -48         # Allocate space for line array (9 words * 4 = 36 bytes) + saved registers
    sw      ra, 44(sp)
    sw      s0, 40(sp)          # s0 = array a
    sw      s1, 36(sp)          # s1 = row counter
    
    addi    s0, a0, 0		# copies the array a containing positions of the queens into s0
    
    # Print the board (8 rows)
    addi    s1, x0, 0           # row = 0, initializes the row counter into s1
    
ps_row_loop:
    # Check if row < 8
    sub     t0, s1, x0          # t0 = row - 0
    addi    t1, x0, 8           # t1 = 8
    sub     t2, s1, t1          # t2 = row - 8
    srai    t3, t2, 31          # t3 = sign bit of (row - 8)
    beq     t3, x0, ps_print_numbers  # if row >= 8, done with board, print numbers SIGN bit is 0
    
    # Initialize line array with '-' (dash) characters
    addi    t0, x0, 0           # t0 = column counter, resets the col counter to 0 at the start of each row
ps_init_line:
    sub     t1, t0, x0          # t1 = column - 0
    addi    t2, x0, 8           # t2 = 8
    sub     t3, t0, t2          # t3 = column - 8
    srai    t4, t3, 31          # t4 = sign bit of (column - 8) (just checking for the MSB)
    beq     t4, x0, ps_place_queen  # if column >= 8, done initializing
    
    slli    t5, t0, 2           # t5 = column * 4
    add     t6, sp, t5          # t6 = address of line[column]
    addi    t1, x0, 45          # t1 = '-' ASCII
    sw      t1, 0(t6)
    
    addi    t0, t0, 1           # column++
    beq     x0, x0, ps_init_line # loop
    
ps_place_queen:
    # Get column position of queen in current row: a[row]
    slli    t0, s1, 2           # t0 = row * 4
    add     t1, s0, t0          # t1 = address of a[row]
    lw      t2, 0(t1)           # t2 = a[row] (column of queen)
    
    # Place '*' at line[a[row]]
    slli    t3, t2, 2           # t3 = column * 4
    add     t4, sp, t3          # t4 = address of line[column] where we have to put the * (queen)
    addi    t5, x0, 42          # t5 = '*' ASCII
    sw      t5, 0(t4)
    
    # Null terminate the line
    addi    t6, x0, 32          # t6 = 32 (offset for line[8]) --> each line takes 4 words * 8 characters in each row = 32
    add     t1, sp, t6          # t1 = address of line[8]
    sw      x0, 0(t1)           # line[8] = 0 (null terminator) (last element add null terminator 0)
    
    # Print the line
    addi    a0, sp, 0           # a0 = address of line array
    jal     ra, my_puts		# prints the strinf present at the address of line array
    
    addi    s1, s1, 1           # row++
    beq     x0, x0, ps_row_loop # always jumps back go ps_row_loop
    
    #--------------------------------------------------------
    # so starting from t0 = 0, we place a dahs (-) as long as the MSB of t0 is 1, i.e. if its less than 8, t0-8 will be negative.
    # once we reach col 7 (from 0, so 8 total), the next one will be 8-8 = 0, MSB = 0. Here we break off the loop and go to ps_place_queen.
    # ps_place_queen  gets col position of queen in current row, places * at line[a[row]]
    #--------------------------------------------------------
    
ps_print_numbers:
    # Print the solution as numbers
    addi    t0, x0, 0           # t0 = index counter
ps_num_loop:			# preparing the looping
    sub     t1, t0, x0          # t1 = index - 0
    addi    t2, x0, 8           # t2 = 8
    sub     t3, t0, t2          # t3 = index - 8
    srai    t4, t3, 31          # t4 = sign bit of (index - 8)
    beq     t4, x0, ps_print_solution  # if index >= 8, done
    
    # Get a[index]
    slli    t5, t0, 2           # t5 = index * 4 (each integer in array is = 4 bytes)
    add     t6, s0, t5          # t6 = address of a[index]
    lw      t1, 0(t6)           # t1 = a[index]
    
    # Convert to ASCII and store in line array
    addi    t2, t1, 48          # t2 = a[index] + '0'
    add     t3, sp, t5          # t3 = address of line[index]
    sw      t2, 0(t3)
    
    addi    t0, t0, 1           # index++
    beq     x0, x0, ps_num_loop
    
ps_print_solution:
    # Null terminate and print the solution line
    addi    t0, x0, 32          # t0 = 32 (offset for line[8]) (start of line + 32 bytes)
    add     t1, sp, t0          # t1 = address of line[8]
    sw      x0, 0(t1)           # line[8] = 0 (store the null terminator at the end)
    
    addi    a0, sp, 0           # a0 = address of line array
    jal     ra, my_puts		# prints the numeric line of numbers
    
    lw      ra, 44(sp)
    lw      s0, 40(sp)		# restore all registers
    lw      s1, 36(sp)
    addi    sp, sp, 48		# restore stack pointer position in the stack
    jalr    x0, ra, 0		# function return

# solve_8queens(a, k) - Main recursive backtracking function
# Input: a0 = array a which stores queen positions thus far, a1 = k (current row) where we wanna place the next queen
# Output: a0 = counter (1 if solution found, 0 otherwise)
solve_8queens:			# reserves 32 bytes on the stack
    addi    sp, sp, -32
    sw      ra, 28(sp)
    sw      s0, 24(sp)          # s0 = array a
    sw      s1, 20(sp)          # s1 = k (current row)
    sw      s2, 16(sp)          # s2 = j (column counter)
    sw      s3, 12(sp)          # s3 = counter
    #function parameters in registers:
    addi    s0, a0, 0
    addi    s1, a1, 0
    addi    s3, x0, 0           # counter = 0
    
    # Check if k == 8 (base case)
    addi    t0, x0, 8
    beq     s1, t0, sq_base_case
    
    # Try each column j from 0 to 7
    addi    s2, x0, 0           # j = 0
    
sq_column_loop:
    # Check if j < 8
    sub     t0, s2, x0          # t0 = j - 0
    addi    t1, x0, 8           # t1 = 8
    sub     t2, s2, t1          # t2 = j - 8
    srai    t3, t2, 31          # t3 = sign bit of (j - 8)
    beq     t3, x0, sq_return   # if j >= 8, done with this level
    
    # Check if position (k, j) is valid
    addi    a0, s0, 0           # a0 = array a
    addi    a1, s1, 0           # a1 = k (row)
    addi    a2, s2, 0           # a2 = j (column)
    jal     ra, is_valid
    
    beq     a0, x0, sq_next_column  # if not valid, try next column
    
    # Place queen at position (k, j): a[k] = j
    slli    t0, s1, 2           # t0 = k * 4
    add     t1, s0, t0          # t1 = address of a[k]
    sw      s2, 0(t1)           # a[k] = j
    
    # Recursive call: solve_8queens(a, k+1)
    addi    a0, s0, 0           # a0 = array a
    addi    a1, s1, 1           # a1 = k + 1
    jal     ra, solve_8queens
    
    add     s3, s3, a0          # counter += recursive result
    
    # If solution found, terminate (first solution only)
    bne     s3, x0, sq_return
    
sq_next_column:
    addi    s2, s2, 1           # j++
    beq     x0, x0, sq_column_loop
    
sq_base_case:
    # k == 8: All queens placed, process solution
    addi    a0, s0, 0           # a0 = array a
    addi    a1, s1, 0           # a1 = k (8)
    jal     ra, process_solution
    # calls process_solution(a,8)
    addi    s3, x0, 1           # counter = 1 (solution found)
    
sq_return:
    addi    a0, s3, 0           # return counter
    
    lw      ra, 28(sp)
    lw      s0, 24(sp)
    lw      s1, 20(sp)
    lw      s2, 16(sp)
    lw      s3, 12(sp)
    addi    sp, sp, 32		# restore stack pointer and all saved registers
    jalr    x0, ra, 0		# function return

### End of your code
#######################

exit:   
        # no need to do anything here
