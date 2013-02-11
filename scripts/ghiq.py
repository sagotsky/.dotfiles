#!/usr/bin/python

# ghiq - GitHub Issue Queue
# <sagotsky@gmail.com>

# CLI app for interacting with your github issue queue

import json, urllib, subprocess, github, re, argparse, getpass, string, os
from os.path import expanduser
from os.path import isfile
from sys import exit
from colortrans import colorprint

# phase 2 (look at auth first)
# -n, --new creates new ticket
# -c, --comment comments on existing ticket

def save_token(authtoken):
  HOME=expanduser('~')
  fh = open(HOME + '/.ghiq', 'w')
  fh.write('authtoken=' + authtoken)
  fh.close()

def cache_tokens(subs):
  dir = expanduser('~') + '/.ghiq.cache/'
  try: 
    os.mkdir(dir)
  except OSError:
    pass

  do_cache = [sub for sub in subs if sub not in os.listdir(dir) and subs[sub].has_issues]
  for sub in do_cache:
    print 'Caching ' + sub + ' for tab completion'
    assignees = ['"'+x.login+'"' for x in subs[sub].get_assignees()]
    labels =    ['"'+x.name+'"' for x in subs[sub].get_labels()]
    milestones =    ['"'+x.title+'"' for x in subs[sub].get_milestones()]
    fh = open(dir + sub, 'w')
    fh.write('users: ' + ' '.join(assignees) + '\n') # can tab complete owners and assignees
    fh.write('labels: ' + ' '.join(labels) + '\n')
    fh.write('milestones: ' + ' '.join(milestones) + '\n')
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
    return ''


def get_subscriptions(user):
  return dict((sub.name, sub) for sub in user.get_subscriptions())

# checks that issue's tokens match currently selected options
def filter_issue_tokens(tokens, options):
  for prop, regex in options.iteritems():

    if prop == 'labels':
      if not filter(regex.match, tokens['__labels']):
        return False
    else: 
      if not re.match(regex, tokens[prop]):
        return False

  return True

# compile a regexx for each filterable property
def compile_filter_regexes(options):
  #re.I?  case sensitivity yay or nay
  filterable = ['assignee', 'labels', 'milestone', 'state', 'body', 'title']
  dict = {}
  for key in filterable:
    if options[key] != '':
      dict[key] = re.compile(options[key])

          
  return dict


# return defaults.  option should be None or read from prefs file (which currently only does token)
def get_defaults():
  defaults = dict()

  #use current git config to get path
  defaults['repo'] = ''
  repo_by_path = get_git_repo()
  if repo_by_path:
    defaults['repo'] = repo_by_path['repo']

  # read in authtoken from file
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
  defaults['assignee'] = '' #get_git_user()

  #no default labels
  defaults['labels'] = ''

  #no default milestone
  defaults['milestone'] = ''
  defaults['title'] = ''
  defaults['body'] = ''

  return defaults

# override defaults with parsed opts from cli
def parse_options():
  defaults = get_defaults()
  if not defaults.has_key('authtoken'):
    defaults['authtoken'] = ''

  parser = argparse.ArgumentParser(description='Interact with a GitHub repository\'s issue queue.')
  
  #parser.add_argument('integers', metavar='N', type=int, nargs='+',
                         #help='an integer for the accumulator')

  parser.add_argument('-r', '--repo', dest='repo', action='store', nargs='?', default=defaults['repo'],
      help='Specify a git repository by name')

  parser.add_argument('-a', '--assignee', dest='assignee', action='store', nargs='?', default=defaults['assignee'],
      help='Filter tickets by assignee.')

  parser.add_argument('-l', '--label', dest='labels', action='store', nargs='?', default=defaults['labels'],
      help='Filter tickets by label(s).')

  parser.add_argument('-m', '--milestone', dest='milestone', action='store', nargs='?', default=defaults['milestone'],
      help='Filter tickets by milestone.')

  parser.add_argument('-t', '--title', dest='title', action='store', nargs='?', default=defaults['title'],
      help='Filter tickets by title text.')

  parser.add_argument('-b', '--body', dest='body', action='store', nargs='?', default=defaults['body'],
      help='Filter tickets by body text.')

  # comment text?  

  parser.add_argument('--auth', dest='auth', action='store', nargs='?', default=defaults['authtoken'],
      help='Authorization token')
# open, closed, all
  parser.add_argument('-s', '--state', dest='state', action='store', nargs='?', default=defaults['state'],
      help='Open/closed state')

# comments (only show up in full body?)
# color (probably has to be -k)
# closed. (-c = shortcut for -s closed)

# search: -s search all, -st search title, -sb search body, -sc search comment.  

# -u update.  maybe user should be assignee?  -o for owner

  parser.add_argument('issue', action='store', default=False, nargs='?', type=int,
      help='GitHub issue number.  (ie. 123)')

  args = parser.parse_args()
  return args

  

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
    gh = github.Github(login, pw) #, user_agent='ghiq.py')
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

def get_issue_tokens(issue):
  tokens = dict(
    number = `issue.number`,
    title = issue.title,
    url = issue.html_url, #url html_url
    state = issue.state,
    body = issue.body,
    assignee = '' if issue.assignee==None else issue.assignee.login,
    #milestone = issue.milestone.title,
    milestone = '' if issue.milestone==None else issue.milestone.title
  )

  #comments = [str(c.body) + "___________" for c in issue.get_comments()],
  comments = []
  for c in issue.get_comments():
    comments.append('- ' + c.user.login + ' -')
    comments.append(c.body.strip() + "\n")
  tokens['comments'] = "\n".join(comments)

  labels = []
  clabels = []
  if issue.labels != None:
    clabels = [colorprint(lbl.color, lbl.name) for lbl in issue.labels]
    labels = [lbl.name for lbl in issue.labels]
  tokens['__labels'] = labels # keep the array around for other functions
  tokens['labels']  = '[' + '] ['.join(labels) + ']'
  tokens['clabels'] = '[' + '] ['.join(clabels) + ']'
  # can lable color be inverted?  or would that be obnoxious

  return tokens

if __name__ == '__main__':
  options = vars(parse_options())

  # login or die trying
  if options['auth']:
    authtoken = options.get('auth')
    gh = github.Github(authtoken) #, user_agent='ghiq.py')
    # shuld probably err here if the authtoken is bad
  else:
    gh = github_login()

  user = gh.get_user()
  subs = get_subscriptions(user)

  #build cache
  cache_tokens(subs)


  #and display issues
  try:
    repo = subs[options['repo']]
  except KeyError:
    print 'Error: You do not have a subscription to ' + `options['repo']` + '.  Did you mean...'
    print ', '.join(subs.keys())
    exit(1)

  # get objects for filters
  query_filters = dict(label='status: needs qa')#,assignee='sagotsky',milestone='Feb')
  match_attr = dict(label='name', assignee='login', milestone='title')
  query = dict()
  for prop,match in query_filters.iteritems():
    matched = False
    fn = 'get_' + prop + 's'
    use_attr = '_'+match_attr[prop]
    for val in getattr(repo, fn)():
      attr = val.__dict__.get(use_attr)
      #if re.match(match, attr):  # this doesn't work.  any labels that match the regex will be used.  but gh will return only issues that have all the labels
      if match == attr:
        if prop == 'label':
          if not query.__contains__('labels'):
            query['labels'] = []
          query['labels'].append(val)

          matched = True
        else:
          query[prop] = val
          matched = True

    if not matched:
      print 'Error: no ' + prop + ' matches ' + match
      exit(1)
    
  if options['issue']:
    # one issue
    fmt = "#{number} {title}\n{url}\n{milestone} {clabels}\n{assignee} - {state}\n{body}\n\n{comments}"
    issue = repo.get_issue(options['issue'])
    tokens = get_issue_tokens(issue)
    print fmt.format(**tokens)
  else:
    # whole queue
    fmt = "#{number} {title}\n{url}\n{milestone} {clabels}\n"
    #for issue in repo.get_issues(assignee='sagotsky'):
    #  this can work, but instead of a string we need to have already fetched a nameduser 
    for issue in repo.get_issues(**query):
      tokens = get_issue_tokens(issue)
      if filter_issue_tokens(tokens, compile_filter_regexes(options)):
        print fmt.format(**tokens)


