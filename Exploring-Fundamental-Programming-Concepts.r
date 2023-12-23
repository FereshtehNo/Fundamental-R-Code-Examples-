#1Calculate the maximum element in an array.
My_array = c(1, 5, 7, 8, 9)

find_max = function(arr) {
  max_value = arr[1]
  for (i in arr) {
    if (i > max_value) {
      max_value = i
    }
  }
  return(max_value)
}

Maximum =find_max(My_array)
print(Maximum)

#2Calculate the minimum element in an array.
My_array = c(1, 5, 7, 8, 9)

find_min = function(arr) {
  min_value = arr[1]
  for (i in arr) {
    if (i <min_value) {
      min_value = i
    }
  }
  return(min_value)
}

Minimum = find_min(My_array)
print(Minimum)

#3Calculate the average of elements in an array.
My_array = c(1, 5, 7, 8, 9)

calc_average = function(arr) {
  sum_value = 0
  count_value = 0
  
  for (i in arr) {
    sum_value = sum_value + i
    count_value = count_value + 1
  }
  
  if (count_value > 0) {
    average_value = sum_value / count_value
    return(average_value)
  } else {
    return(NULL)
  }
}

ave = calc_average(My_array)
print(ave)

#4Count the number of elements in an array.
My_array = c(1, 5, 7, 8, 9)
count_element= function(arr){
  counting=0
  for (i in arr) {
    counting=counting+1
    
  }
  return(counting)
}
count_num= count_element(My_array)
print(count_num)

#5Reverse an array
My_array = c(1, 5, 7, 8, 9)

reverse_array = function(arr) {
  n = length(arr)
  reversed_arr = numeric(n)
  
  for (i in 1:n) {
    reversed_arr[i] = arr[n - i + 1]
  }
  
  return(reversed_arr)
}

reversed_result <- reverse_array(My_array)
print(reversed_result)

#6Sort array
My_array = c(1, 5, 3, 8, 9)
bubble_sort =function(arr) {
  n = length(arr)
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        temp = arr[j]
        arr[j] = arr[j + 1]
        arr[j + 1] = temp
      }
    }
  }
  return(arr)
}

sorting <- bubble_sort(My_array)
print(sorting)
#recursive approach
merge_and_sort = function(arr1, arr2) {
  combined = c(arr1, arr2)
  return(sort(combined))
}

quick_sort = function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Pivot is the first element of the input array arr. 
  pivot = arr[1]
  smaller = arr[arr < pivot]
  equal = arr[arr == pivot]
  larger = arr[arr > pivot]
  
  # Recursively apply quicksort to smaller and larger subarrays
  sorted_smaller = quick_sort(smaller)
  sorted_larger = quick_sort(larger)
  
  # Concatenate the sorted subarrays with the pivot
  result = c(sorted_smaller, equal, sorted_larger)
  
  return(result)
}

a = c(1, 4, 9, 12, 25)
b = c(2, 3, 8, 11, 29)

# Merge and sort arrays a and b using quicksort
final_result = quick_sort(merge_and_sort(a, b))
print(final_result)



#7Find the second-largest element in an array.
My_array <- c(1, 5, 7, 10, 9)
largest <- -Inf
second_largest <- -Inf

for (i in My_array) {
  if (i > largest) {
    second_largest = largest
    largest = i
    #If i is not greater than largest but is greater than the current second_largest, it means i is the new second-largest element.
  } else if (i > second_largest) {
    second_largest = i
  }
}

print(second_largest)

#8Find the second-smallest element in an array.
My_array = c(1, 5, 7, 8, 9)
smallest = Inf
second_smallest = Inf

for (i in My_array) {
  if (i < smallest) {
    second_smallest <- smallest
    smallest = i
  } else if (i < second_smallest) {
    
    second_smallest = i
  }
}

print(second_smallest)

#9Calculate the factorial of a number.
calculate_factorial= function(n){
  result=1
  for(i in 1:n){
    result= result*i
    if(i==0){
      return(1)
    }
  }
  return(result)
}
fac_5= calculate_factorial(7)
print(fac_5)

#10Check if a number is prime.
pimre= function(n){
  if(n<=1){
    return(FALSE)
  }
  for (i in 2:floor(sqrt(n))){
    if (n%% i==0){
      return(FALSE)
    }
    
    
  }
  return(TRUE)
}
prime_num5= pimre(12)
print(prime_num5)

#11Calculate the Fibonacci sequence up to a certain number.
#dynamic
Fibonacci= function(n){
  fib= numeric(n)
  fib[1]=0
  fib[2]=1
  for(i in 3:n){
    fib[i]= fib[i-1]+fib[i-2]
  }
return(fib)


}
fibo=Fibonacci(3)
print(fibo)

#recursion
fib=function(n){
  if(n<=1){
    return(n)
    
  }else{
    return(fib(n-1)+fib(n-2))
  }
}
fib3=fib(3)
print(fib3)

#Fibonacci Q-Matrix
Fibonacci_q_matrix = function(n) {
  qmatrix = matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  # Define the matrix exponentiation function
  matrix_power = function(A, k) {
    if (k == 0) {
      return(diag(nrow(A)))  #  the diag() function is used to create a diagonal matrix or extract the diagonal elements from a matrix
    } else if (k %% 2 == 1) {
      return(A %*% matrix_power(A, k - 1))#%*% operator is used for matrix multiplication
      
    } else {
      half_power = matrix_power(A, k / 2)
      return(half_power %*% half_power)
    }
  }
  
  # Get the n-th power of the Q-Matrix
  result_matrix = matrix_power(qmatrix, n)
  
  # The Fibonacci number is the element at (1, 1) of the result matrix
  fibonacci_result = result_matrix[1, 1]
  
  # Print the result for better visibility
  cat("Fibonacci(", n, ") =", fibonacci_result, "\n")
  
  # Return the Fibonacci number
  return(fibonacci_result)
}

# Example: Calculate and print Fibonacci series up to the 10th term
for (i in 0:9) {
  Fibonacci_q_matrix(i)
}

#12Remove duplicates from an array.
My_array = c(1, 5, 7, 8, 9, 9, 9)

remove_duplicate = function(arr) {
  result = numeric(0)
   for (i in arr) {
    if (!(i %in% result)) {
      result = c(result, i)
    }
  }
  
  return(result)
}

remove_dup = remove_duplicate(My_array)
print(remove_dup)

#13 Implement a binary search algorithm.
My_array = c(1, 12, 8, 7, 9, 9, 9)
target_element = 12
My_array = sort(My_array)

binary_search = function(arr, target) {
  left = 1 #In a typical binary search implementation, the variable left is initialized to 1 because, in most programming languages, array indices start from 0.
  right = length(arr)
  
  while (left <= right) {
    mid = (left + right) %/% 2
    
    if (arr[mid] == target) {
      return(mid)
    } else if (arr[mid] < target) {
      left = mid + 1
    } else {
      right = mid - 1
    }
  }
  #target element is not present in the array
  return(-1)
}

result = binary_search(My_array, target_element)
print(result)


#14 list in R
my_list=c(
  numeric=c(1,2,3,4), 
  string=c("a","b","c"),
  matrix=matrix(1:5, nrow=5, ncol=5),
  data_frame=data.frame(name=c("Fereshte","Zahra"), age=c(27,25)),
  other=c(TRUE, FALSE)
)
print(my_list)

#15Calculate the power of a number using recursion.
power_func= function(base, exponent){
  if(exponent==0){
    return(1)
    
  }else{
    return(base*power_func(base, exponent-1))# base by itself (exponent - 1) times
    #power_func(2, 3) calls 2 * power_func(2, 2), 2×(2×(2×1))
    
    
    
    #power_func(2, 2) calls 2 * power_func(2, 1)
    #power_func(2, 1) calls 2 * power_func(2, 0)
  }
  
}
base=2
exponent=3
result=power_func(base, exponent)
print(result)

#16 Implement a simple calculator.
my_calculator = function(x, operator, y) {
  if (operator == "+") {
    return(x + y)
  } else if (operator == "-") {
    return(x - y)
  } else if (operator == "*") {
    return(x * y)
  } else if (operator == "/") {
    if (y != 0) {
      return(x / y)
    } else {
      return("error")
    }
  }
  return(NULL)  
}

x = NA
while (is.na(x)) {
  x = as.numeric(readline(prompt = "Enter x: "))
  if (is.na(x)) {
    cat("Invalid input. Please enter a numeric value for x.\n")
  }
}

operator = readline(prompt = "Enter operator: ")

y = NA
while (is.na(y)) {
  y = as.numeric(readline(prompt = "Enter y: "))
  if (is.na(y)) {
    cat("Invalid input. Please enter a numeric value for y.\n")
  }
}


result = my_calculator(x, operator, y)
print(result)

#17Function to separate odd and even numbers in an array
My_array = c(1, 5, 7, 8, 9, 9, 9)
Separate_odd_even= function(arr){
  even_num= arr[arr%%2==0]
  odd_num= arr[arr%%2 !=0]
    
  return(list(even=even_num, odd=odd_num))
}
result=Separate_odd_even(My_array)
print(result)

#18 create matrix
my_matrix= list(c(1,2,3,5),c(4,5,6,7),c(1,2,3,4))

for (i in 1:length(my_matrix)) {
  print(my_matrix[[i]])
  
}

#19 Add two matrix
matrix1= list(c(1,2,3),c(4,5,6))
matrix2= list(c(1,2,5),c(4,3,6))
result_matrix=list(c(0,0,0),c(0,0,0))
for (i in 1:length(matrix1)) {
  for (j in 1:length(matrix2[[i]])) {
    result_matrix[[i]][j]= matrix1[[i]][j]+ matrix2[[i]][j]
    
  }
  
}
for (i in 1:length(result_matrix)) {
  print(result_matrix[[i]])
  
}

#20Subtract two matrices.
matrix1= list(c(1,2,3),c(4,5,6))
matrix2= list(c(1,2,5),c(4,3,6))
# starts with a known state.
result_matrix=list(c(0,0,0),c(0,0,0))
for (i in 1:length(matrix1)) {
  for (j in 1:length(matrix2[[i]])) {#In the given code, the double square brackets [[]] are used when accessing elements of lists, whereas the single square brackets [] are used when accessing elements of vectors or matrices.
    
    result_matrix[[i]][j]= matrix1[[i]][j]- matrix2[[i]][j]
    
  }
  
}
for (i in 1:length(result_matrix)) {
  print(result_matrix[[i]])
  
}
#21 Multiply two matrices.
matrix1 = list(c(1, 2, 3), c(4, 5, 6))
matrix2 = list(c(1, 2, 5), c(4, 3, 6))

if (length(matrix1) != length(matrix2)) {
  stop("Error")
}
#creates a list of the same length as matrix1 to store the individual matrices resulting from the multiplication
result_matrix = vector("list", length = length(matrix1))

for (i in 1:length(matrix1)) {
  result_matrix[[i]] = numeric(length(matrix2[[1]]))#Since, in a matrix, all rows have the same length, it's sufficient to check the length of the first row to know the dimensions of the matrix.
  for (j in 1:length(matrix2[[1]])) {
    for (k in 1:length(matrix1[[i]])) {#k is a loop variable representing the index used to iterate over the elements of the rows in matrix1[[i]] and matrix2[[i]] during the matrix multiplication.
      result_matrix[[i]][j] = result_matrix[[i]][j] + matrix1[[i]][k] * matrix2[[i]][k]
    }
  }
}




for (i in 1:length(result_matrix)) {
  print(result_matrix[[i]])
}

#22 Plot multiple lines on the same chart.
x= 1:10
y= 1:10
y1= x
y2=x*2
y3= x/2
plot(x,y, type="l", col="blue", lty=1, xlab="x", ylab="y", main="multiple lines on the same chart")
lines(x, y1,lty=1, col="red" )
lines(x, y2, lty=1, col="orange")
lines(x, y3, lty=1, col="pink")
legend("topleft", legend=c("y1","y2","y3"),lty=1:3, col=c("red","orange","pink"))

#23Create a bar chart.used to display and compare the values of different categories or groups
categories=c("A","B","c")
value= c(10,20,30)
barplot(value, col="light green", xlab="categories", ylab="value")

#24Generate a histogram.used to represent the distribution of a continuous variable.
data=c(8,30,15,20,33,4.8)
hist(data, col="brown", xlab="value", ylab="frequency",main="Histogram")

#25Plot a scatter plot
x=c(5,8,7,9)
y=c(1,2,9,5)
plot(x,y, col="purple", pch=20, xlab="x", ylab="y", main="scatter plot")

#26Plot a pie chart
categories=c("A","B","c")
value= c(10,20,30)
pie(value, labels="categories", col=rainbow(length(categories)), main="pie chart")

#27 Heatmap
xsize=10
ysize=10
x=0: xsize+1
y= 0: ysize+1
plot(x,y, type="n", axes=TRUE, ann= TRUE)#"n" means no plotting
colors=c("red","blue","green","pink","orange")
sampledata=runif(xsize*ysize)#The length of this vector is equal to the total number of elements in the matrix
datamatrix= matrix(sampledata,nrow = xsize,ncol =ysize )
for(x in 1:xsize){
  for (y in 1:ysize) {
    xcoords=c(x,x,x+1,x+1)
    ycoords=c(y, y+1, y+1,y)
    index=datamatrix[x,y]*length(colors) - 1 +1#Subtracts 1 from the scaled value. This is likely done to ensure that the resulting index is zero-based since indices in R typically start from 1.then Adds 1 to the previous result. This adjustment is made to convert the zero-based index to the 1-based index used in R.
    color=colors[index]
    polygon(xcoords,ycoords,col=color)
  }
}
pdf("heatmap.pdf")
dev.off()

#28 box plot
data= list(a=c(25,30,40,50),
           b=c(15,50,62,40,33),
           c=c(20,25,33,69))
boxplot(data,col=c("red","blue","pink"),
names=c("group A","group B","group C"),main="box plot", xlab="group", ylab="value")

#29 Two Sum problem
nums = c(1, 2, 1,4)
target = 6

two_sum = function(nums, target) {
  nums_indicate = numeric()#To store the indices of the two numbers that sum up to the target.
  hash_table = numeric()#A The hash_table list is initialized to store key-value pairs.
  for (i in 1:length(nums)) {
    complement = target - nums[i]
    
    if (complement %in% names(hash_table)) {
      nums_indicate = c(hash_table[[as.character(complement)]], i)
      break
    }
    
    hash_table[[as.character(nums[i])]] = i #Adds the current number and its index to the hash table
  }
  
  return(nums_indicate)
}

result = two_sum(nums, target)
print(result)

#30 square root function without using the built-in sqrt function
x=9
sqrt_calc= function(x){
  if(x==0){
    return(0)
  }else if( x==1){
    return(1)
    
  }
  guess= x/2
  tolerance= 1e-10 #The loop will continue until the difference between the square of guess and x is smaller than this tolerance
  while (abs(guess^2 -x)> tolerance) {
    guess=  0.5 * (guess + x / guess)
  }
  return(guess)
}
result=sqrt_calc(x)
print(result)

#31 Determining Reachability in a 2-D Plane with K Steps
x = 0
y = 0
k = 2
reach_piont= function(x,y,k){
  if((x + y + k) %% 2 == 0 && k >= abs(x) + abs(y)){
    return("yes")
  }else{
    return("no")
  }
  
}
result=reach_piont(x,y,k)
print(result)

#32 Problem 3: Saddle point
example_matrix= matrix(c(1, 2, 3, 4, 5,
                         6, 7, 8, 9, 10,
                         11, 12, 13, 14, 15,
                         16, 17, 18, 19, 20,
                         21, 22, 23, 24, 25), nrow = 5, ncol = 5)
saddle_point= function(matrix){
  rows=nrow(matrix)
  cols=ncol(matrix)
  for(i in 1:rows){
    for(j in 1:cols){
      value=matrix[i,j]
      max_row=max(matrix[i,])
      min_col=min(matrix[,j])
      if (value >=max_row &&value<= min_col ){
        saddle_points= c(saddle_point,list(c(i,j)))
      }
    }
  }
  return(saddle_points)
}
find_saddle=saddle_point(example_matrix)
print(find_saddle)

#33 Decimal to binary number
decimal_to_binary = function(decimal_num) {
  binary_result = integer(32)
  i = 1
  
  while (decimal_num > 0) {
    binary_result[i] = decimal_num %% 2
    decimal_num = decimal_num %/% 2
    i = i + 1
  }
  
  
  return(binary_result)
}

decimal_num = 42
result = decimal_to_binary(decimal_num)
print(result)

#34 prime_num_with_limit less than 10^9
prime = function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  for (i in 2:floor(sqrt(n))) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

find_primes_up_to_limit = function(limit) {
  primes = (2:limit)[sapply(2:limit, prime)]
  return(primes)
}

limit = 10^9
result = find_primes_up_to_limit(limit)
print(result)

#35Merge and sort 
quick_sort = function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  # Pivot is the first element of the input array arr. 
  pivot = arr[1]
  smaller = arr[arr < pivot]
  equal = arr[arr == pivot]
  larger = arr[arr > pivot]
  
  # Recursively apply quicksort to smaller and larger subarrays
  sorted_smaller = quick_sort(smaller)
  sorted_larger = quick_sort(larger)
  
  # Concatenate the sorted subarrays with the pivot
  result = c(sorted_smaller, equal, sorted_larger)
  
  return(result)
}

a = c(1,12, 4, 9, 15, 25)
b = c(2, 3, 8, 11, 29)

# Merge and sort arrays a and b using quicksort
final_result = quick_sort(c(a, b))
print(final_result)


#36 Problem 1- Binary to Decimal
my_array=c(1,1,1)
binary_to_decimal= function(my_array){
  decimal=0
  binary_digit= as.numeric(my_array)
  for( i in 1:length(binary_digit)){
    decimal=decimal*2 + binary_digit[i]
  }
  return(decimal)
}
result= binary_to_decimal(my_array)
print(result)

#37-Problem 2- Top K Values from n Number
my_array = c(1, 7, 5, 12, 4)
K = 2
bubble_sort =function(arr) {
  n = length(arr)
  for (i in 1:(n - 1)) {
    for (j in 1:(n - i)) {
      if (arr[j] > arr[j + 1]) {
        temporary = arr[j]
        arr[j] = arr[j + 1]
        arr[j + 1] = temporary
      }
    }
  }
  return(arr)
}
get_top_k_values= function(my_array,K){
  sorted_arr=bubble_sort(my_array)
  top_k_values=sorted_arr[(length(sorted_arr) - K + 1):length(sorted_arr)]
  return(top_k_values)
}
result1=bubble_sort(my_array)
result2=get_top_k_values(my_array,K)
print(result1)
print(result2)

#38-Problem 3- K-mer Matching
sequence1 = "ATCGTAC"
sequence2= "GTACGTA"
kmer= 3
find_common_kmers= function(seq1, seq2, k){
  create_kmers=function(sequence,k){
    kmers=character()
    for (i in 1:(nchar(sequence) - k + 1)) {
      kmer = substr(sequence, i, i + k - 1)
      kmers = c(kmers, kmer)
    }
    return(kmers)
  }
  
  k_mers_seq1=create_kmers(sequence1,k)
  k_mers_seq2=create_kmers(sequence2,k)
  common_kmers = character()
  for (kmer1 in k_mers_seq1) {
    for (kmer2 in k_mers_seq2) {
      if (kmer1 == kmer2) {
        common_kmers <- c(common_kmers, kmer1)
        break
      }
    }
  }
  
  return(common_kmers)
}
result= find_common_kmers(sequence1, sequence2, kmer)
print(result)

#39-Identify high variance columns
A=matrix(sample(1000, 100), ncol = 200, nrow = 5)
col_variances= apply(A, 2, var)
print(col_variances)
top_k_variances = order(col_variances, decreasing = TRUE)[1:2]
print(top_k_variances)

#40:Array Rotation
array_rotation = function(arry, k) {
  n = length(arry)
  result = numeric(n)
  for (i in 1:n) {
    result[(i + k - 1) %% n + 1] = arry[i]  # by search
  }
  return(result)
}

my_array = c(1, 3, 2, 4, 5)
k = 3
final_result = array_rotation(my_array, k)
print(final_result)

#41: palindrome check
my_string = c("ACBCA")
convert_string_to_vector = strsplit(my_string, NULL)[[1]]#by search
my_array = c(convert_string_to_vector)
print(my_array)
palindrome = function(arr) {
  n = length(arr)
  for (i in 1:(n/2)) {  # n/2 is by search
    if (arr[i] != arr[n - i + 1]) {
      return(FALSE)
    }
  }
  return(TRUE)
}

result= palindrome(my_array)
print(result)

##42: Sum of digit
sum_of_digit=function(x){
  sum=0
  while(x> 0){
    digit= x %% 10
    sum= sum+ digit
    x= x %/% 10
  }
  return(sum)
  
}
num=125
result=sum_of_digit(num)
print(result)

#43 reverse string
my_string = c("ACBCAD")
convert_string_to_vector = strsplit(my_string, NULL)[[1]]
my_array = c(convert_string_to_vector)
print(my_array)
reverse_string = function(x){
  n = length(x)
  reversed = character(n)  
  for(i in 1:n){
    reversed[i] = x[n-i+1]
  }
  return(c(reversed))
}

result = reverse_string(my_array)
print(result)







