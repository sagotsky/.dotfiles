#!/usr/bin/python

# sapi parser
# class generalizes service apis
# should let you plug some options into an object, then get and parse the result.

from sys import argv
import argparse

class SapiParser:
  def __init__(self):
    self.parameters = []
    self.paths = []
    self.ops = {} 
    self.argparse = argparse.ArgumentParser(description='Sapi Parser')

  def url(self, url):
    self.url = url

  # oed uses querys - url?foo=bar&baz=bob etc
  # github uses dirs - api/repos/user/watched
  # how to support both?  mixture?  
  # different types of params?
  def addParam(self, param):
    self.parameters.append(param)


  def query(self):
    s = self.url
    for path in self.paths:
      if path in self.ops:
        s += '/' + self.ops[path]
    
    s += '?'
    for param in self.parameters:
      if param in self.ops:
        s += param + '=' + self.ops[param] + '&'

    print s

  def addArg(self, arg, target, value, help_text):
    self.argparse.add_argument(arg, dest=target, help=help_text)
    self.ops[target] = value

  def parseArgs(self):
    self.argparse.parse_args()

oed = SapiParser()
oed.url('http://www.oed.com/srupage')
#oed.addPath('foo/bar/baz') # no path in this example
oed.addParam('operation') # why not just take array of params?
oed.addParam('query')

#oed.addCondition('true', 'operation', 'searchRetrieve')
#oed.addCondition('true', 'query', argv[1])
# maxiumumRecords, startRecord # oed paging.  ignoring this for now


oed.addArg('--op', 'operation', 'val', 'help text')
oed.parseArgs()

oed.query()
#http://www.oed.com/srupage?operation=searchRetrieve&query=cql.serverChoice+=+test&maximumRecords=100&startRecord=1
                                                           #wtf?
