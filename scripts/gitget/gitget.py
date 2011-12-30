#!/usr/bin/python

# gitget
# command line tool for searching and downloading from github

import json
import urllib
import subprocess
from optparse import OptionParser
from getpass import getuser

# url of github api
api = 'https://api.github.com/'

# get results from github url.  format them.  return array full of output texts
def callapi(url):
  s = urllib.urlopen(url)
  response = json.loads( s.read() )
  out = []
  for item in response:
    t = ''
    for term in format(options):
      if term in item:
        t += item[term]
      else:
        t += term
    out.append({'repo':item['name'], 'txt':t, 'git_url':item['git_url']})

  s.close()
  return out

# get format array to show which items to print
def format(options):
  formats = {
      'list'      : ['git_url', '\n'],
      'default'   : ['name', '\n', 'description', '\n', 'git_url', '\n'],
      'verbose'   : ['name', '\n', 'description', '\n', 'git_url', '\nLast Push: ', 'pushed_at', '\n'], 
      }

  #if (options.list):
    #return ['git_url', '\n']
  
  if not options.verbosity in formats:
    options.verbosity = 'default'
    
  return formats[ options.verbosity ]


  #return ['name', '\n', 'description', '\n', 'git_url', '\n\n']

# figure out what url suffix to use
def getUrl(options):
  url_suffixes = {
      'watched'   : 'users/' + options.user + '/watched',
      'repos'     : 'users/' + options.user + '/repos',
  }

  return api + url_suffixes[options.method]

# main
if __name__ == '__main__':
  # get options from cli
  usage = 'usage: %prog [options] <search_term(s)>'
  description='Search your watched or owned repositories on github.  Provide optional search terms to match strings.'
  description+='example: %prog --repos --user torvalds kernel'
  parser = OptionParser(usage=usage, description=description)

  parser.add_option('-w', '--watch', '--watched',
      action='store_const',
      const='watched',
      dest='method',
      help='Show repositories watched by user'
      )

  parser.add_option('-r', '--repos', 
      action='store_const',
      const='repos',
      dest='method',
      help='Show repositories owned by user'
      )

  parser.add_option('-u', '--user',
      dest='user',
      help='Specifies a GitHub username.  Otherwise uses your local username.',
      default=getuser()
      )

  parser.add_option('-l', '--list',
      dest='verbosity',
      action='store_const',
      const='list',
      help='Only list the URL of the git repositories.'
      )

  parser.add_option('-v', '--verbose',
      dest='verbosity',
      action='store_const',
      const='verbose',
      help='Show additional information about each repository',
      )

  parser.add_option('-c', '--clone',
      dest='clone',
      action='store_true',
      help='Clone matched repositories.'
      )

# --dest, --install

  (options, args) = parser.parse_args()


  url = getUrl(options)
  response = callapi(url)

  # drop results that don't match args terms
  for repo in response:
    keep = True
    for arg in args:
      if not arg in repo['txt']:
        keep = False

    if keep:
      print repo['txt']
      if options.clone:
        subprocess.call(['git', 'clone', repo['git_url'] ])

    
    
