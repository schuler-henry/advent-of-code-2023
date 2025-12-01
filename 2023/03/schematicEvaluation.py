def is_symbol(char):
    return not (char.isdigit() or char == '.' or char == '\n')

def is_symbol_at_indices(line, indices):
    for index in indices:
        if is_symbol(line[index]):
            return True
    return False

def is_star(char):
    return char == '*'

def get_star_position(line, indices, row_index):
    positions = []
    for index in indices:
        if is_star(line[index]):
            positions.append((row_index, index))
    return positions

def main():
    lines = []
    valid_numbers = []
    gear_candidates = {}

    with open('/home/henry/Documents/Code/advent-of-code-2023/03/input.txt', 'r') as f:
        for line in f:
            lines.append(line)

    for row_index, line in enumerate(lines):
        number_string = ""
        indices = []
        previous_row_index = row_index - 1 if row_index > 0 else 0
        next_row_index = row_index + 1 if row_index < len(lines) - 1 else len(lines) - 1
        for column_index, char in enumerate(line):
            # check if char is digit
            if char.isdigit():
                print(f'Found digit {char} at row {row_index} and column {column_index}')
                number_string += char
                indices.append(column_index)
            else:
                if number_string != "":
                    print(f'Found number {number_string} at row {row_index} and columns {indices}')

                    # check if number is valid
                    previous_index = indices[0] - 1 if indices[0] > 0 else 0
                    next_index = indices[-1] + 1 if indices[-1] < len(line) - 1 else len(line) - 1
                    indices.append(previous_index)
                    indices.append(next_index)
                    if (is_symbol(line[previous_index]) or is_symbol(line[next_index]) or is_symbol_at_indices(lines[previous_row_index], indices) or is_symbol_at_indices(lines[next_row_index], indices)):
                        print(f'Number {number_string} is valid')
                        valid_numbers.append(int(number_string))

                        # Check whether the symbol is a star and get its index
                        star_positions = []
                        if is_star(line[previous_index]):
                            star_positions.append((row_index, previous_index))
                        elif is_star(line[next_index]):
                            star_positions.append((row_index, next_index))
                        else:
                            star_positions += get_star_position(lines[previous_row_index], indices, previous_row_index)
                            star_positions += get_star_position(lines[next_row_index], indices, next_row_index)
                        
                        if len(star_positions) > 0:
                            print(f'Found star at {star_positions}')
                            for star_position in star_positions:
                                if gear_candidates.get(star_position) is None:
                                    gear_candidates[star_position] = [int(number_string)]
                                else:
                                    gear_candidates[star_position] += [int(number_string)]
                                # gear_candidates.append([int(number_string), star_position])

                    number_string = ""
                    indices = []

    print(f'Valid numbers: {valid_numbers}')
    # Calculate result
    result = sum(valid_numbers)
    print(f'Result: {result}')

    print(f'Gear candidates: {gear_candidates}')
    gear_result = 0
    for gear_candidate in gear_candidates:
        if len(gear_candidates[gear_candidate]) == 2:
            gear_result += gear_candidates[gear_candidate][0] * gear_candidates[gear_candidate][1]
    print(f'Gear result: {gear_result}')


if __name__ == '__main__':
    main()