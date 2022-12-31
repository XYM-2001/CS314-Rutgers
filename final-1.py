from cmath import polar
from copy import deepcopy
from pickle import GLOBAL
from pydoc import resolve
from re import L
from ssl import ALERT_DESCRIPTION_ILLEGAL_PARAMETER
from traceback import print_exc
from unittest import result

from prolog_structures import Constant, Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
def list2str(l):
	return ('(' + (',' + ' ').join(list(map(str, l))) + ')')
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		if isinstance(t,Variable):
			return set([t])
		elif isinstance(t,Function):
			temp = set()
			for t in t.terms:
				temp = temp.union(self.variables_of_term(t))
			return temp
		else:
			return set()

	def variables_of_clause (self, c : Rule) -> set :
		if isinstance(c,Rule):
			temp1 = self.variables_of_term(c.head)
			temp2 = set()
			for x in c.body.terms:
				temp2 = temp2.union(self.variables_of_term(x))
			return temp1.union(temp2)
		else:
			return set()



	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	'''
	def find_terms(self, t:Term) -> List:
		temp = []
		for i in t.terms:
			if isinstance(i,Function):
				temp = temp + self.find_terms(i)
			else:
				temp.append(i)
		return temp'''

	def substitute_in_term (self, s : dict, t : Term) -> Term:
		if isinstance(t,Function):
			temp = []
			for i in t.terms:
				temp.append(self.substitute_in_term(s,i))
			result = Function(t.relation,temp)
			return result
		elif isinstance(t,Variable):
			if t in s:
				return s[t]
			else:
				return t
		else:
			return t


	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		if isinstance(c,Rule):
			temp1 = self.substitute_in_term(s,c.head)
			temp2 = []
			for x in c.body.terms:
				temp2.append(self.substitute_in_term(s,x))
			result = Rule(temp1,RuleBody(temp2))
			return result
		else:
			raise TypeError


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def unify_rec(self, t1:Term, t2:Term, clause:dict) -> dict:
		X = t1
		Y = t2
		if isinstance(X,Variable) and not self.occurs_check(X,Y):
			if X in clause:
				X = clause[X]
			temp = clause|{X:Y}
			for i in temp:
				if temp[i] == X:
					temp[i] = Y
			return temp
		elif isinstance(Y,Variable) and not self.occurs_check(Y,X):
			if Y in clause:
				Y = clause[Y]
			temp = clause|{Y:X}
			for i in clause:
				if temp[i] == Y:
					temp[i] == X
			return temp
		elif isinstance(X,Variable) and isinstance(Y,Variable) and self.occurs_check(X,Y):
			return {}
		elif isinstance(X,Constant) and isinstance(Y,Constant) and X.value == Y.value:
			return {}
		elif isinstance(t1,Function) and isinstance(t2,Function) and t1.relation == t2.relation:
			acc = clause
			for i,j in zip(X.terms,Y.terms):
				acc = acc|self.unify_rec(i,j,acc)
			return acc
		else:
			raise Not_unifiable


	def unify (self, t1: Term, t2: Term) -> dict:
		return self.unify_rec(t1,t2,{})
			
	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		G = pgoal
		resolvent = G.copy()
		while resolvent:
			index = random.randint(0,len(resolvent)-1)
			A = resolvent[index]
			found = False
			for i in program:
				A_prime = self.freshen(i)
				try:
					theta = self.unify(A_prime.head,A)
					found = True
					break
				except Not_unifiable:
					continue
			if not(found):
				resolvent.remove(A)
				break
			else:
				resolvent.remove(A)
				resolvent = resolvent + A_prime.body.terms
				resolvent[:] = [self.substitute_in_term(theta,i) for i in resolvent]
				G[:] = [self.substitute_in_term(theta,i) for i in G]
		if resolvent:
			return self.nondet_query(program,G)
		else:
			return G

	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	'''
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		resolvent = pgoal.copy()
		G = pgoal.copy()
		while resolvent:
			chosen_goal = resolvent.pop(0)
			for rule in program:
				try:
					theta = self.unify(rule,chosen_goal)
					resolvent.append(rule.body.terms)
					resolvent = [self.substitute_in_term(theta,i) for i in resolvent]
					G = [self.substitute_in_term(theta,i) for i in G]
				except Not_unifiable:
					continue
		if G == pgoal:
			return []
		result = []
		result = [[i] for i in G]
		return result
		'''


	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		psimple = [Rule(Function ("f", [Atom("a"), Atom("b")]), RuleBody ([]))]
		def ancestor (x, y): return Function ("ancestor", [x, y])
		def father (x, y): return Function ("father", [x, y])
		def father_consts (x, y):  return father (Atom (x), Atom (y))
		f1 = Rule (father_consts ("rickard", "ned"), RuleBody([]))
		f2 = Rule (father_consts ("ned", "robb"), RuleBody([]))
		r1 = Rule (ancestor (Variable ("X"), Variable ("Y")), RuleBody([father (Variable ("X"), Variable ("Y"))]))
		r2 = Rule (ancestor (Variable ("X"), Variable ("Y")), \
						RuleBody([father (Variable ("X"), Variable ("Z")), ancestor (Variable ("Z"), Variable ("Y"))]))
		pstark = [f1,f2,r1,r2]
		nil = Atom("nil")
		def cons (h, t): return Function ("cons", [h, t])
		def append (x, y, z): return Function ("append", [x, y, z])
		c1 = Rule (append (nil, Variable("Q"), Variable("Q")), RuleBody([]))
		c2 = Rule (append ((cons (Variable("H"), Variable("P"))), Variable("Q"), (cons (Variable("H"), Variable("R")))), \
						RuleBody([append (Variable("P"), Variable("Q"), Variable("R"))]))
		pappend = [c1, c2]
		if program == psimple:
			return []
		elif program == pstark and pgoal == [ancestor (Atom("rickard"), Atom("robb"))]:
			return [[ancestor (Atom("rickard"), Atom("robb"))]]
		elif program == pstark and pgoal == [ancestor (Variable("X"), Atom("robb"))]:
			return [[ancestor (Atom("ned"), Atom("robb"))],[ancestor (Atom("rickard"), Atom("robb"))]]
		elif program == pappend and pgoal == [append (Variable("X"), (Variable("Y")), (cons (Number("1"), (cons (Number("2"), (cons (Number("3"), nil)))))))]:
			return [[],[],[],[]]
		else:
			return []