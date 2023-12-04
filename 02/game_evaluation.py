def get_game_id(line):
    id_string = line.split(':')[0].split(' ')[1]
    return int(id_string)

def get_games(line):
    return line.split(':')[1].split(';')

def get_max_color_count(games):
    color_count = {
        'red': 0,
        'green': 0,
        'blue': 0
    }

    for game in games:
        colors = game.split(',')
        for color in colors:
            (count, color_name) = color.strip().split(' ')
            if color_count[color_name] < int(count):
                color_count[color_name] = int(count)

    return color_count


def main():
    print('Game Evaluation 1')
    bag_count = {
        'red': 12,
        'green': 13,
        'blue': 14
    }
    valid_game_ids = []
    print('Valid games have max of {} red, {} green, and {} blue cubes.'.format(bag_count['red'], bag_count['green'], bag_count['blue']))
    with open('input.txt', 'r') as f:
        for line in f:
            max_color_count = get_max_color_count(get_games(line))

            if (max_color_count['red'] <= bag_count['red'] and
                max_color_count['green'] <= bag_count['green'] and
                max_color_count['blue'] <= bag_count['blue']):

                print('Game {} is valid.'.format(get_game_id(line)))
                valid_game_ids.append(get_game_id(line))

    print('Valid game ids: {}'.format(valid_game_ids))

    valid_game_id_sum = sum(valid_game_ids)
    print('Sum of valid game ids: {}'.format(valid_game_id_sum))


    game_powers = []
    print('Game Evaluation 2')
    with open('input.txt', 'r') as f:
        for line in f:
            max_color_count = get_max_color_count(get_games(line))

            power = max_color_count['red'] * max_color_count['green'] * max_color_count['blue']
            game_powers.append(power)

    print('Game powers: {}'.format(game_powers))

    game_power_sum = sum(game_powers)
    print('Sum of game powers: {}'.format(game_power_sum))



if __name__ == '__main__':
    main()