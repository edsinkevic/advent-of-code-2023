visited = set()

def find_adj_numbers(matrix, _i, _j):
    nums = []
    for (i, j) in adj_indices(_i, _j):
        v = get_at(matrix, i, j)
        if v.isnumeric():
            start_j = go_to_start(matrix, i, j)
            if (i, start_j) not in visited:
                num = read_number(matrix, i, start_j)
                nums.append(num)
                visited.add((i, start_j))
    return nums 

def go_to_start(matrix, i, j):
    curr = '0'
    while isnumber(curr):
        j -= 1
        curr = get_at(matrix, i, j)
    return j + 1

def read_number(matrix, i, j):
    curr = get_at(matrix, i, j)
    accum = ""
    while isnumber(curr):
        j += 1
        accum += curr
        curr = get_at(matrix, i, j)
    return int(accum)

def get_at(matrix, i, j):
    try:
        return matrix[i][j]
    except IndexError:
        return None

def adj_indices(i, j):
    return [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1),
            (i + 1, j + 1), (i - 1, j + 1), (i + 1, j - 1),
            (i - 1, j - 1)]

def isnumber(x):
    return x is not None and x.isnumeric()


with open("data.txt", "r") as f:
    matrix = [*map(list, f.read().splitlines())]

current_sum1 = 0
current_sum2 = 0

for i in range(len(matrix)):
    for j in range(len(matrix[0])):
        v = get_at(matrix, i, j)
        if not v.isnumeric() and v != ".":
            nums = find_adj_numbers(matrix, i, j)
            if len(nums) == 2 and v == "*":
                ratio = nums[0] * nums[1]
                current_sum2 += ratio
            current_sum1 += sum(nums)

print(current_sum1)
print(current_sum2)

