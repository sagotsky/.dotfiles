#!/usr/bin/python

# sapi parser
# class generalizes service apis
# should let you plug some options into an object, then get and parse the result.

class parser:
  def __init__(self):
    self.parameters = []

  def url(self, url):
    self.url = url

  def addParam(self, param):
    self.parameters.append(param)

  def query(self):
    s = self.url + '?'
    for param in self.parameters:
      s += param + '=5&'
    print s

oed = parser()
oed.url('http://www.oed.com/srupage')
oed.addParam('operation')
oed.addParam('searchRetrieve')
oed.addParam('query')
oed.query()
# maxiumumRecords, startRecord # oed paging.  ignoring this for now
