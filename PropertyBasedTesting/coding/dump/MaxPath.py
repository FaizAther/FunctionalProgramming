# Definition for a binary tree node.
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    def __str__(self):
        if self == None:
            return "None"
        else:
            return "( " + self.left.__str__() + " " + \
                    self.val.__str__() + " " + \
                    self.right.__str__() + " )"

class Solution:
	def maxPathSum(self, root: TreeNode) -> int:
		max_path = float("-inf")
		def get_max_gain(node):
			nonlocal max_path
			if node is None:
				return 0

		gain_on_left = max(get_max_gain(node.left), 0)
		gain_on_right = max(get_max_gain(node.right), 0)

		current_max_path = node.val + gain_on_left + gain_on_right
		max_path = max(max_path, current_max_path)

		return node.val + max(gain_on_left, gain_on_right)

def genTreeLR(n, m):
	if n <= 0:
		return TreeNode(m,None,None)
	else:
		t = genTreeLR(n-1, m)
		return TreeNode(m, t,t)

root = genTreeLR(5, 1)
node = root

def main():
	sol = Solution()
	max_path = sol.maxPathSum(root)
	print(max_path)
	print(genTreeLR(2,1))
	return max_path

if "__name__" != "__main__":
	main()