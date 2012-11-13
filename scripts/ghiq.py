#!/usr/bin/python

# ghiq - GitHub Issue Queue
# <sagotsky@gmail.com>

# CLI app for interacting with your github issue queue

import json, urllib, subprocess, github, re, argparse, getpass 
from os.path import expanduser
from os.path import isfile
from sys import exit
from colortrans import colorprint
import string

# -u, --user    Filter by user(s)
# -l, --label   Filter by label(s)
# -r, --repo    Filter by repository
# By default, user and repo will be read from the current .git.  ~/.gitconfig as well?
# git config --get github.user
# git config --get remote.origin.url
# Negation for filters?


# phase 1
# No arg lists tickets
# Ticket id arg shows that ticket in full

# phase 2 (look at auth first)
# -n, --new creates new ticket
# -c, --comment comments on existing ticket

# group by?  counts?

#( other supported parameters: milestone, state, creator)

# tab completion can be achieved by saving user, label into into cahce file.
# -vv, -q, -qq, etc limit how much info is shown


def save_token(token):
  HOME=expanduser('~')
  fh = open(HOME + '/.ghiq', 'w')
  fh.write('token=' + token)
  fh.close()

def get_git_user():
  return subprocess.Popen(['git', 'config', '--get', 'github.user'], stdout=subprocess.PIPE).communicate()[0].strip()
  
def get_git_repo():
  repo = subprocess.Popen(['git', 'config', '--get', 'remote.origin.url'], stdout=subprocess.PIPE).communicate()[0]
  repo = re.sub(r'.*:(.*).git', r'\1', repo)
  repo = repo.strip().split('/')
  if len(repo) == 2:
    return dict(owner=repo[0], repo=repo[1])
  else: 
    return False


def get_subscriptions(user):
  subs = dict() 
  for sub in user.get_subscriptions():
    subs[sub.name] = sub
  return subs


def filter_issue_by_labels(labels, options):
  if options['label'] == '':
    return True 

  search = options['label'].split(',') 
  for label in labels:
    if search.__contains__(label.name):
      return True
    if search.__contains__('^'+label.name):
      return False

  return False

def filter_issue_by_state(state, options):
  return (state == 'all' or (state == options['state']))

def filter_issue_by_assignee(user, options):
  if user == None:
    return (options['assignee'] == 'None')

  if user.login == options['assignee']:
    return True

  return False

def filter_issue(issue, options):
  functions = globals()
  for prop,value in issue.__dict__.iteritems():
    func = 'filter_issue_by' + prop
    if functions.has_key(func):
      filtered = functions[func](value, options)
      if filtered == False:
        return False
  return True


# return defaults.  option should be None or read from prefs file (which currently only does token)
def get_defaults():
  defaults = dict()

  #use current git config to get path
  defaults['repo'] = ''
  repo_by_path = get_git_repo()
  if repo_by_path:
    defaults['repo'] = repo_by_path['repo']

  # read in token from file
  HOME=expanduser('~')
  file = HOME + '/.ghiq'
  if isfile(file):
    fh = open(HOME + '/.ghiq', 'r')
    for line in fh.readlines():
      prop, value = line.split('=', 1)
      defaults[prop] = value
    fh.close()

  # default open state
  defaults['state'] = 'open'

  #user gets current git user
  defaults['assignee'] = get_git_user()

  #no default label
  defaults['label'] = ''

  return defaults

# override defaults with parsed opts from cli
def parse_options():
  defaults = get_defaults()
  if not defaults.has_key('token'):
    defaults['token'] = ''

  parser = argparse.ArgumentParser(description='Interact with a GitHub repository\'s issue queue.')
  
  #parser.add_argument('integers', metavar='N', type=int, nargs='+',
                         #help='an integer for the accumulator')

  parser.add_argument('-r', '--repo', dest='repo', action='store', nargs='?', default=defaults['repo'],
      help='Specify a git repository by name')

  parser.add_argument('-u', '--user', dest='assignee', action='store', nargs='?', default=defaults['assignee'],
      help='Filter tickets by assignee(s).  If multiple, separate usernames with commas.')

  parser.add_argument('-l', '--label', dest='label', action='store', nargs='?', default=defaults['label'],
      help='Filter tickets by label(s).')

  parser.add_argument('--auth', dest='auth', action='store', nargs='?', default=defaults['token'],
      help='Authorization token')
# open, closed, all
  parser.add_argument('-s', '--state', dest='state', action='store', nargs='?', default=defaults['state'],
      help='Authorization token')

# comments (only show up in full body?)
# color (probably has to be -k)
# closed. (-c = shortcut for -s closed)

  return parser.parse_args()

  

# no token, try in with username/password
def github_login():
  # get credentials
  login = getpass.getuser()
  login2 = raw_input('Username ['+login+']: ')
  if len(login2):
    login = login2

  # log in
  try:
    pw = getpass.getpass()
    gh = github.Github(login, pw)
  #except GithubException as e:
    #print 'Could not log in'
  except Exception as e:
    print 'Could not log in'
    exit(1)

  # create and store auth token
  make_token = raw_input('Store authorization token for future logins? y/[n] ')
  if make_token == 'y':
    auth = gh.get_user().create_authorization()
    save_token(auth.token)

  return gh



if __name__ == '__main__':
  options = vars(parse_options())

  # login or die trying
  if options['auth']:
    token = options.get('auth')
    gh = github.Github( token )
    # shuld probably err here if the token is bad
  else:
    gh = github_login()

  user = gh.get_user()

  subs = get_subscriptions(user)

  try:
    repo = subs[options['repo']]
  except KeyError:
    print 'Error: You do not have a subscription to ' + `options['repo']` + '.  Did you mean...'
    print ', '.join(subs.keys())
    exit(1)

  fmt = "#{number} {title}\n{url}\n{labels}\n"

  for issue in repo.get_issues():
    if filter_issue(issue, options):

      tokens = dict()
      tokens['number'] = issue.number
      tokens['title'] = issue.title
      tokens['url'] = issue.url
      tokens['state'] = issue.state
      tokens['body'] = issue.body
      tokens['assignee'] = issue.assignee.login
      tokens['comments'] = issue.comments

      labels = []
      if issue.labels != None:
        for label in issue.labels:
          labels.append( colorprint(label.color, label.name) )
      tokens['labels'] = '[' + '] ['.join(labels) + ']'
      # can lable color be inverted?  or would that be obnoxious

      # any other useful tokens?  maybe timestamps?  is there a piont in using those
      # without having sortability
      
      print fmt.format(**tokens)
      
  #for repo in user.get_repos():
    #print repo.full_name





