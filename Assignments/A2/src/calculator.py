import numpy as np
import sys 
import warnings

warnings.filterwarnings('ignore')

def get_operation_index(equation):
    '''
    Description: Acquires the indices of operators, including spaces, from the input list
    Parameters: equation --> List
    Output: operation_index --> List
    '''
    numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']
    operation_index = []
    for index, num in enumerate(list(equation)):
        if equation[index] not in numbers:
            operation_index.append(index)
    return operation_index

def extract_numbers(equation, operation_index):
    ''' 
    Description: Extracts the digits from the input list
    Parameters: equation --> List, operation_index --> List
    Output: values (float values from the input) --> List
    '''
    start = 0
    values = []
    for index in operation_index:
        if start == 0:
            values.append(equation[start:index])
            start = index + 1
        values.append(equation[start:index])
        start = index + 1
    if index != len(equation):
        values.append(equation[index + 1:len(equation)])
    values = list(filter(None, values))
    values = [float(num) for num in values]
    return values

def check_equation(equation):
    ''' 
    Description: Checks or change the input list of equation based on the properties of mathematical operators 
    Parameters: equation --> List, Array 
    Output: List, Array
    '''
    for index, value in enumerate(equation):
        operations = ['+', '-', '*', 'x', '/']
        if equation[index] == '/':
            assert (equation[index + 1] != float(0) or equation[index + 1] != float(0)), 'Solution is undefined. No division by zero!'
        if equation[index] in operations:
            assert (equation[index + 1] not in operations), 'Wrong equation. No operator should be succeeded by another operator exluding parentheses.' 
        if equation[index] == 'x':
            equation[index] = '*'
    return equation


def create_new_equation(equation, values):
    ''' 
    Description:    Creates an array to consolidate all the extracted values and operators
                    from the input string. It also ensures that array has the same size with
                    the string.
    Parameters:     equation --> List/Array
                    values   --> List
    Output:         new_equation --> List/Array
    '''
    new_equation = list(np.zeros(len(equation)))
    numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']
    for index, value in enumerate(equation):
        if value not in numbers:
            new_equation.insert(index, value)
    new_equation = new_equation[0:len(equation)]
    new_equation = list(filter(lambda a: a != ' ', new_equation))

    if len(new_equation) > 3:
        for index in range(len(new_equation) - 1):
            if index != 0:
                if new_equation[index] == float(0) and new_equation[index + 1] == float(0):
                    new_equation[index] = '#'
            else:
                new_equation[index] = '#'
    
    new_equation = list(filter(lambda a: a != '#', new_equation)) 
    get_num_index = [index for index, value in enumerate(new_equation) if value == float(0)]
    replacement = dict(zip(get_num_index, values))
    for replace, value in zip(replacement.keys(), replacement.values()):
        new_equation[replace] = float(value)
    return new_equation

def get_index_parenthesis(new_equation):
    ''' 
    Description:    Extracts the indices of all parentheses from the new_equation
    Parameters:     new_equation --> LIst
    Output:         location --> List
                    check --> Boolean
    '''
    location = {}
    for index, item in enumerate(new_equation):
        if new_equation[index] == '(' and isinstance(new_equation[index + 1], (np.int, np.float)) is True:
            location[index] = new_equation.index(')', index) + 1
    if len(location) > 0:
        check = 1
    else:
        check = 0
    return location, check

def evaluate_parenthesis(equation, start, end):
    ''' 
    Description:    Calculates expression within parenthesis
    Parameters:     equation --> List
                    start    --> int
                    end      --> int
    Output:         answer   --> float
    '''
    return eval(''.join(map(str, equation[start:end])))

def calculate_multiply(equation):
    ''' 
    Description:    Multiplies values
    Parameters:     equation --> List
    Output:         answer   --> Float
    '''
    for index, item in enumerate(equation):
        if index != len(equation) - 1:
            if equation[index] == '*':
                answer = equation[index - 1] * equation[index + 1]
    return answer

def calculate_divide(equation):
    ''' 
    Description:    Divides values
    Parameters:     equation --> List
    Output:         answer   --> Float
    '''
    for index, item in enumerate(equation):
        if index != len(equation) - 1:
            if equation[index] == '/':
                answer = equation[index - 1] / equation[index + 1]
    return answer

def calculate_add(equation):
    ''' 
    Description:    Add values
    Parameters:     equation --> List
    Output:         answer   --> Float
    '''
    for index, item in enumerate(equation):
        if index != len(equation) - 1:
            if equation[index] == '+':
                answer = equation[index - 1] + equation[index + 1]
    return answer

def calculate_subtract(equation):
    ''' 
    Description:    Subtract values
    Parameters:     equation --> List
    Output:         answer   --> Float
    '''
    for index, item in enumerate(equation):
        if index != len(equation) - 1:
            if equation[index] == '-':
                answer = equation[index - 1] - equation[index + 1]
    return answer

def check_right_asterisk(new_equation):
    ''' 
    Description: (3 + 5)2 == (3 + 5)*2
    Parameters:
        new_equation
    Output: new_equation, verify (booolean)
    '''
    for index, value in enumerate(new_equation):
        if index != len(new_equation) - 1:
            if new_equation[index] == ')' and isinstance(new_equation[index + 1], (float)) is True:
                new_equation.insert(index + 1, '*')
                verify = True
            else:
                verify = False
    return new_equation, verify 

def check_left_asterisk(new_equation):
    ''' 
    Description: 2(3 + 5) == 2*(3 + 5)
    Parameters:
        new_equation
    Output: new_equation, verify (booolean)
    '''
    for index, value in enumerate(new_equation):
        if index != len(new_equation) - 1:
            if isinstance(new_equation[index], (float)) is True and new_equation[index + 1] == '(':
                new_equation.insert(index + 1, '*')
                verify = True
            else:
                verify = False
    return new_equation, verify


def check_first_digit(new_equation):
    ''' 
    Description:    Verifies if first digit is a number 
    Parameters:     new_equation --> List
    Output:         new_equation --> List
    '''
    operations = ['+', '-', '*', 'x', '/', ' ']
    numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']
    if new_equation[0] in numbers and new_equation[1] in operations:
        new_equation = '0' + new_equation
    return new_equation

def delete_comma(equation):
    ''' 
    Description:    Deletes comma for any financial values e.g., 12,000
    Parameters:     equation --> List
    Output:         equation --> List
    '''
    equation = equation.replace(',', '')
    return equation
    
def main(equation, display = False):
    '''
    Sample equation = '((((65 + 40) * 2) - 10) / 15) - 4 * 20 + 10 * (50) - 2 / (4 + 5 * 10)'
    Description:    Main function fof the calculator 
    Parameters:     equation --> List
                    display  --> Boolean
    Output:         answer   --> Float
    '''
    equation = check_first_digit(equation)
    equation = delete_comma(equation)
    operation_index = get_operation_index(equation)
    values = extract_numbers(equation, operation_index)
    new_equation = create_new_equation(equation, values)
    new_equation = check_equation(new_equation)
    new_equation, verify_right = check_right_asterisk(new_equation)
    new_equation, verify_left  = check_left_asterisk(new_equation)
    while True:
        if ('(' or ')') in new_equation:
            location, check = get_index_parenthesis(new_equation)
            answer = {}
            for key, value in zip(location.keys(), location.values()):
                answer[key, value] = evaluate_parenthesis(new_equation, key, value)
            for key, value in zip(answer.keys(), answer.values()):
                new_equation[key[0]] = value
                for idx in range(key[0] + 1, key[1]):
                    new_equation[idx] = '#'
            new_equation = list(filter(lambda a: a != '#', new_equation))  
            if display is True:
                print(np.array(new_equation))
        else:
            if new_equation.count('(') != new_equation.count(')'):
                new_equation = np.insert(new_equation, 0, '(')
            answer = round(eval(''.join(map(str, new_equation))), 3)
            if verify_right is True or verify_left is True:
                if round(eval(''.join(map(str, equation))), 3) != answer:
                    answer = round(eval(''.join(map(str, equation))), 3)
            break
    return answer
    
if __name__ == "__main__":
    print('Insert your string mathematical expression: ')
    equation = input()
    print('====================================================')
    answer = main(equation)
    print(f'Answer: {answer}')
    sys.exit()