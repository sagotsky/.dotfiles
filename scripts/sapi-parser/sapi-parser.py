#!/usr/bin/python

# sapi parser
# class generalizes service apis
# should let you plug some options into an object, then get and parse the result.

from sys import argv

class parser:
  def __init__(self):
    self.parameters = []

  def url(self, url):
    self.url = url

  # oed uses querys - url?foo=bar&baz=bob etc
  # github uses dirs - api/repos/user/watched
  # how to support both?  mixture?  
  # different types of params?
  def addParam(self, param):
    self.parameters.append(param)


  def query(self):
    s = self.url + '?'
    for param in self.parameters:
      s += param + '=5&'
    print s

oed = parser()
oed.url('http://www.oed.com/srupage')
oed.addParam('operation') # why not just take array of params?
oed.addParam('query')

oed.addCondition('true', 'operation', 'searchRetrieve')
oed.addCondition('true', 'query', argv[1])
oed.query()
# maxiumumRecords, startRecord # oed paging.  ignoring this for now
