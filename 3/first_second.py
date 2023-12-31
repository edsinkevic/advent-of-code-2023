def main():
    with open("data.txt", "r") as f:
        matrix = f.read().splitlines()

    current_sum1 = 0
    current_sum2 = 0
    visited = set()

    for i in range(len(matrix)):
        for j in range(len(matrix[0])):
            v = get_at(matrix, i, j)
            if not v.isnumeric() and v != ".":
                nums = find_adj_numbers(matrix, i, j, visited)
                if len(nums) == 2 and v == "*":
                    ratio = nums[0] * nums[1]
                    current_sum2 += ratio
                current_sum1 += sum(nums)

    print(current_sum1)
    print(current_sum2)
    assert current_sum1 == 527446, 'Incorrect answer in the first part'
    assert current_sum2 == 73201705, 'Incorrect answer in the second part'



def find_adj_numbers(matrix, _i, _j, visited):
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
    while isnumber(curr := get_at(matrix, i, j)):
        j -= 1
    return j + 1


def read_number(matrix, i, j):
    accum = ""
    while isnumber(curr := get_at(matrix, i, j)):
        j += 1
        accum += curr
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


main()
