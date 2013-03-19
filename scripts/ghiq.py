#!/usr/bin/python

# ghiq, but with classes

import argparse, subprocess, re, github, os
from os.path import expanduser
from os.path import isfile
from sys import exit
from colortrans import colorprint
from github import GithubObject


"""
GitHubProperty classes
----------------------

These generalize how to get items and lists of items from GitHub objects.

Each one implements the following: 
  repo_get_all() - returns all strings in repo of this type
  issue_get_text() - returns text for this prop from a single issue

The following functions can be overridden:
  repo_get_prop() - Searches list of props, returns list of matches
  set_opts() - Gives class a chance to respond to cli opts
  format() - Format token for display

The following properties are also available
  filterable - Indicates whether this property can be filtered after downloading issues
  api_query - Indicates that this property can be sent to github's api to restrict which issues download
  cache - Store the text from this property so it can be tab completed.
"""
class GitHubProp(object):
  filterable = False
  api_query = False
  cache = False

  def repo_get_prop(self, repo, id):
    for (txt,object) in self.repo_get_all(repo).iteritems():
      if re.match(id, txt):
        return object

  def set_opts(self, options):
    pass

  def format(self, token):
    return token

class BodyProp(GitHubProp):
  cli_flags = ['-b', '--body']
  help = 'Filter tickets by body.'
  name = 'body'
  filterable = True

  def issue_get_text(self, issue, match=None):
    if match == None or re.match(match, issue.body):
      return issue.body

  def repo_get_all(self, repo):
    return False #dict((x.login, x) for x in repo.get_assignees())

class TitleProp(GitHubProp):
  cli_flags = ['-t', '--title']
  help = 'Filter tickets by title.'
  name = 'title'
  filterable = True

  def issue_get_text(self, issue, match=None):
    if match == None or re.match(match, issue.title):
      return issue.title

  def repo_get_all(self, repo):
    return False #dict((x.login, x) for x in repo.get_assignees())

class AssigneeProp(GitHubProp):
  cli_flags = ['-a', '--assignee']
  help = 'Filter tickets by assignee.'
  name = 'assignee'
  filterable = True
  api_query = True
  cache = True

  def issue_get_text(self, issue, match=None):
    txt = '' if issue.assignee==None else issue.assignee.login
    if match == None or re.match(match, txt):
      return txt

  def repo_get_all(self, repo):
    return dict((x.login, x) for x in repo.get_assignees())

class StateProp(GitHubProp):
  cli_flags = ['-s', '--state']
  help = 'Filter tickets by open/closed state.'
  name = 'state'
  filterable = True
  api_query = True

  def issue_get_text(self, issue, match=None):
    txt = '' if issue.state==None else issue.state
    if match == None or re.match(match, txt):
      return txt

  def repo_get_all(self, repo):
    return dict(open='open', closed='closed')


class OnwerProp(GitHubProp):
  cli_flags = ['-o', '--owner']
  help = 'Filter tickets by owner.'
  name = 'user'
  filterable = True

  def issue_get_text(self, issue, match=None):
    txt = '' if issue.user==None else issue.user.login
    if match == None or re.match(match, txt):
      return txt

  def repo_get_all(self, repo):
    return dict((x.login, x) for x in repo.get_subscribers())

class MilestoneProp(GitHubProp):
  cli_flags = ['-m', '--milestone']
  help = 'Filter tickets by milestone.'
  name = 'milestone'
  filterable = True
  api_query = True
  cache = True

  def issue_get_text(self, issue, match=None):
    txt = '' if issue.milestone==None else issue.milestone.title
    if match == None or re.match(match, txt):
      return txt
    
  def repo_get_all(self, repo):
    return dict((m.title, m) for m in repo.get_milestones())
    

class LabelsProp(GitHubProp):
  cli_flags = ['-l', '--label']
  help = 'Filter by labels'
  name = 'labels'
  filterable = True
  api_query = True
  cache = True
  color = True

  def issue_get_text(self, issue, match=None):
    labels = []
    for label in issue.get_labels():
      if label is not None and (match == None or any(re.match(match, lbl) for lbl in labels)):
        if self.color:
          labels.append(colorprint(label.color, label.name))
        else:
          labels.append(label.name)
    return labels

  def repo_get_all(self, repo):
    return dict((l.name, l) for l in repo.get_labels())

  def repo_get_prop(self, repo, id):
    labels = []
    for label in repo.get_labels():
      if re.match(id, label.name):
        labels.append(label)
    if len(labels):
      return labels
    else:
      return None

  def set_opts(self, options):
    color = options['color_labels']

  def format(self, labels):
    text = '] ['.join(labels)
    if len(text):
      return '[' + text + ']'
    return ''

"""
Assorted helper functions
"""

# caches tab completable tokens
def cache_tokens(subs, properties):
  dir = expanduser('~') + '/.ghiq.cache/'
  try: 
    os.mkdir(dir)
  except OSError:
    pass

  # gets list of repositories that need caching
  do_cache = [sub for sub in subs if sub not in os.listdir(dir) and subs[sub].has_issues]
  for sub in do_cache:
    text = ''
    for prop in properties: 
      if prop.cache:
        text += prop.name + ': "' + '" "'.join(prop.repo_get_all(subs[sub]).keys()) + '"\n'

    print 'Caching ' + sub + ' for tab completion'
    fh = open(dir + sub, 'w')
    fh.write(text)
    fh.close()

# gets repository of cwd
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

# returns string values of issue for printing.  these are not the props defined as githubprops
def get_issue_tokens(issue):
  tokens = dict(
    number = `issue.number`,
    title = issue.title,
    url = issue.html_url, #url html_url
    state = issue.state,
    body = issue.body,
  )

  comments = []
  for c in issue.get_comments():
    comments.append('- ' + c.user.login + ' -')
    comments.append(c.body.strip() + "\n")
  tokens['comments'] = "\n".join(comments)

  #labels = []
  #clabels = []
  #if issue.labels != None:
    #clabels = [colorprint(lbl.color, lbl.name) for lbl in issue.labels]
    #labels = [lbl.name for lbl in issue.labels]
  #tokens['__labels'] = labels # keep the array around for other functions
  #tokens['labels']  = '[' + '] ['.join(labels) + ']'
  #tokens['clabels'] = '[' + '] ['.join(clabels) + ']'
  # can lable color be inverted?  or would that be obnoxious

  return tokens

# return defaults.  option should be None or read from prefs file (which currently only does token)
def get_defaults():
  defaults = dict(
    repo = '',
    authtoken = '',
    color_labels = True,
  )

  #use current git config to get path
  repo_by_path = get_git_repo()
  if repo_by_path:
    defaults['repo'] = repo_by_path['repo']

  # read in authtoken from file
  HOME = expanduser('~')
  file = HOME + '/.ghiq'
  if isfile(file):
    fh = open(HOME + '/.ghiq', 'r')
    for line in fh.readlines():
      prop, value = line.split('=', 1)
      defaults[prop] = value
    fh.close()

  return defaults

"""
Main
"""

if __name__ == '__main__':
  # instantiate all property classes
  properties = []
  for sub in vars()['GitHubProp'].__subclasses__():
    constructor = globals()[sub.__name__]
    properties.append(constructor())

  # build and parse cli opts
  parser = argparse.ArgumentParser(description='Interact with a GitHub repository\'s issue queue.')
  defaults = get_defaults()


  parser.add_argument('-r', '--repo', dest='repo', action='store', nargs='?', default=defaults['repo'],
      help='Specify a git repository by name')
  parser.add_argument('--auth', dest='auth', action='store', nargs='?', default=defaults['authtoken'],
      help='Authorization token')
  parser.add_argument('--color', dest='color_labels', action='store', nargs='?', default=defaults['color_labels'],
      help='Use color labels')
  # issue number?
  for prop in properties:
    parser.add_argument(*prop.cli_flags, help=prop.help, action='store', nargs='?', default='', dest=prop.name)

  options = vars(parser.parse_args())

  # login or die trying
  if options['auth']:
    authtoken = options.get('auth')
    gh = github.Github(authtoken) #, user_agent='ghiq.py')
  else:
    gh = github_login()

  user = gh.get_user()
  subs = get_subscriptions(user)

  #build cache
  cache_tokens(subs, properties)

  #and display issues
  try:
    repo = subs[options['repo']]
  except KeyError:
    print 'Error: You do not have a subscription to ' + `options['repo']` + '.  Did you mean...'
    print ', '.join(subs.keys())


  # set options and build api query from filters
  query = dict()
  for prop in properties:
    prop.set_opts(options)

    if prop.api_query and options[prop.name] == None:     # property must have no value
      query[prop.name] = GithubObject.NotSet
    elif prop.api_query and len(options[prop.name]):      # property matches value
      query[prop.name] = prop.repo_get_prop(repo, options[prop.name])
      if query[prop.name] == None:
        print 'Filter error: ' + prop.name + ' can not match "'+options[prop.name]+'"'
        exit(1)
      if type(query[prop.name]) == list and len(query[prop.name]) > 1:
        del query[prop.name] # api can't take multiple args.  just filter afterwards


  # print issues
  fmt = "#{number} {title}\n{url}\n{milestone} {labels}\n"
  #for issue in repo.get_issues(assignee='sagotsky'):
  #  this can work, but instead of a string we need to have already fetched a nameduser 

  for issue in repo.get_issues(**query):
    tokens = get_issue_tokens(issue)
    show_issue = True

    # get text for all properties
    for prop in properties: 
      matches = True
      if options[prop.name]:
        matches = prop.issue_get_text(issue, options[prop.name]) # post query filters
      if matches == None or matches == '' or matches == []:
        show_issue = False
      else: 
        # finally, don't print issues that have negatedvalues
        tokens[prop.name] = prop.issue_get_text(issue)
        if options[prop.name] == None and len(tokens[prop.name]) > 0:
          show_issue = False
        else:
          tokens[prop.name] = prop.format(tokens[prop.name]) # format for display

    if show_issue:
      print fmt.format(**tokens)


