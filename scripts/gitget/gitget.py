#!/usr/bin/python

# gitget
# command line tool for searching and downloading from github

import json
import urllib
import subprocess
import sys
from optparse import OptionParser
from getpass import getuser


# url of github api
api_v3 = 'https://api.github.com/'
api_v2 = 'http://github.com/api/v2/json/'
api = api_v3

# get results from github url.  format them.  return array full of output texts
def callapi(url):
  s = urllib.urlopen(url)
  response = json.loads( s.read() )
  out = []

  #more v2 adjustment
  if 'repositories' in response:
    response = response['repositories']

  for item in response:
    if item == 'error':
      print response['error']
      break
    t = ''
    # update v2 keys
    if 'url' in item:
      item['git_url'] = item['url']

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
      'default'   : ['name', ' :: ', 'description' ],
      'verbose'   : ['name', '\n', 'description', '\n', 'git_url', '\nLast Push: ', 'pushed_at', '\n'], 
      }
# use str.format instead

  if not options.verbosity in formats:
    options.verbosity = 'default'
    
  return formats[ options.verbosity ]


  #return ['name', '\n', 'description', '\n', 'git_url', '\n\n']

# figure out what url suffix to use
def getUrl(options, args):
  url_suffixes = {
      'watched'   : 'users/' + options.user + '/watched',
      'repos'     : 'users/' + options.user + '/repos',
  }

  if options.method:
    return api + url_suffixes[options.method]
  else:
    return api_v2 + 'repos/search/' + '+'.join(args)

# main
if __name__ == '__main__':
  # show usage if no args are provided
  if len(sys.argv) < 2:
    sys.argv.append('--help')

  # get options from cli
  usage = 'usage: %prog [options] <search_term(s)>'
  description='Search repositories on github.  Provide optional search terms to match strings.'
  epilog='Example: gitget.py --repos --user torvalds kernel'
  parser = OptionParser(usage=usage, description=description, epilog=epilog)

  parser.add_option('-w', '--watch', '--watched',
      action='store_const',
      const='watched',
      dest='method',
      help='Show all repositories watched by user'
      )

  parser.add_option('-r', '--repos', 
      action='store_const',
      const='repos',
      dest='method',
      help='Show all repositories owned by user'
      )

  parser.add_option('-u', '--user',
      dest='user',
      help='Specifies a GitHub username when fetching watched or owned repositories.  Otherwise uses your local username.',
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

# --dest

  (options, args) = parser.parse_args()


  url = getUrl(options, args)
  print url
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

    
    
