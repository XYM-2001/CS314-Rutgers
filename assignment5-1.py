from typing import List
from functools import reduce
import sys
import traceback

#################
### Problem  1 ##
#################

def closest_to (l, v):
    min_diff = abs(v-l[0])
    result = l[0]
    for x in l:
        if abs(v-x) < min_diff:
            min_diff = abs(v-x)
            result = x
    return result
#################
### problem  2 ##
#################

def assoc_list (l):
    temp = [(i,l.count(i)) for i in l]
    result = []
    for i in temp:
        if i not in result:
            result.append(i)
    return result

#################
### Problem  3 ##
#################

def buckets (f, l):
    result = []
    for i in l:
        newlist = True
        for j in result:
            if f(j[0],i):
                j.append(i)
                newlist = False
        if newlist:
            result.append([i])
    return result

###################################
# Definition for a binary tree node
###################################

class TreeNode:
    def __init__(self, val=None, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    # construct tree from a list of values `ls`
    def list_to_tree(self, ls):
        self.val = self.left = self.right = None # clear the current tree

        if not ls: # ls is None or l == []
            return # tree is None

        i = 0
        self.val = ls[i]
        queue = [self]
        while queue: # while queue is not empty
            i += 1
            node = queue.pop(0)
            if node.val is None:
                continue

            if 2*i -1 >= len(ls) or ls[2*i-1] is None:
                pass
            else:
                node.left = TreeNode(ls[2*i-1])
                queue.append(node.left)

            if 2*i >= len(ls) or ls[2*i] is None:
                pass
            else:
                node.right = TreeNode(ls[2*i])
                queue.append(node.right)


#################
### Problem  4 ##
#################
def level_order(root: TreeNode):
    curr_list = []
    if root.left is not None:
        curr_list.append(root.left)
    if root.right is not None:
        curr_list.append(root.right)
    result = [[root.val]]
    while curr_list:
        temp = []
        curr_level = curr_list.copy()
        for i in curr_level:
            temp.append(i.val)
            if i.left is not None:
                curr_list.append(i.left)
            if i.right is not None:
                curr_list.append(i.right)
            curr_list.remove(i)
        result.append(temp)
    return result

#################
### Problem  5 ##
################# 
def pathSum(root: TreeNode, targetSum: int) -> List[List[int]]:
    paths = []
    def Sum_helper_rec(root: TreeNode, currpath: List[int]):
        nonlocal paths
        temp = currpath.copy()
        if root is None:
            return
        elif root.left is None and root.right is None:
            temp.append(root.val)
            paths.append(temp)
            return
        else:
            temp.append(root.val)
            if root.left is not None:
                Sum_helper_rec(root.left,temp)
            if root.right is not None:
                Sum_helper_rec(root.right,temp)
            return
    Sum_helper_rec(root,[])
    for x in paths:
        if sum(x) != targetSum:
            paths.remove(x)
    return paths

#################
### Test cases ##
#################
def main():
    print ("Testing your code ...")
    error_count = 0

    # Testcases for Problem 1
    try:
        assert (closest_to([2,4,8,9],7) == 8)
        assert (closest_to([2,3,7,9],5) == 3)
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
        # tb_info = traceback.extract_tb(tb)
        # filename, line, func, text = tb_info[-1]
        # print('An error occurred on line {} in statement {}'.format(line, text))
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 2
    try:
        result = assoc_list([1, 2, 2, 1, 3])
        result.sort(key=lambda x:x[0])
        assert (result == [(1,2), (2, 2), (3, 1)])

        result = assoc_list(["a","a","b","a"])
        result.sort(key=lambda x:x[0])
        assert (result == [("a",3), ("b",1)])

        result = assoc_list([1, 7, 7, 1, 5, 2, 7, 7])
        result.sort(key=lambda x:x[0])
        assert (result == [(1,2), (2,1), (5,1), (7,4)])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 3
    try:
        assert (buckets (lambda a, b : a == b, [1,2,3,4]) == [[1], [2], [3], [4]])
        assert (buckets (lambda a, b : a == b, [1,2,3,4,2,3,4,3,4]) == [[1], [2, 2], [3, 3, 3], [4, 4, 4]])
        assert (buckets (lambda a, b : a % 3 == b % 3, [1,2,3,4,5,6]) == [[1, 4], [2, 5], [3, 6]])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    ### Specify 3 trees for testing problems 4 & 5
    root_1 = TreeNode()
    root_1.list_to_tree([5,4,8,11,None,13,4,7,2,None,None,5,1])

    root_2 = TreeNode()
    root_2.list_to_tree([1,2,3])

    root_3 = TreeNode()
    root_3.list_to_tree([1,2])

    # Testcases for Problem 4
    try:
        assert (level_order(root_1) == [[5], [4, 8], [11, 13, 4], [7, 2, 5, 1]])
        assert (level_order(root_2) == [[1], [2, 3]])
        assert (level_order(root_3) == [[1], [2]])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    # Testcases for Problem 5
    try:
        assert (pathSum(root_1, 22) == [[5, 4, 11, 2], [5, 8, 4, 5]])
        assert (pathSum(root_2, 4) == [[1, 3]])
        assert (pathSum(root_3, 0) == [])
    except AssertionError as err:
        error_count += 1
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)
    except:
        error_count += 1
        print("Unexpected error:", sys.exc_info()[0])
        _, _, tb = sys.exc_info()
        traceback.print_tb(tb)

    print (f"{error_count} out of 5 programming questions are incorrect.")

main()