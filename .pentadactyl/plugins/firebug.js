/**
 * VERSION: 0.1
 * AUTHOR: Maksim Ryzhikov
 * Plugin to interact with firebug.
 * Based on plugin firebug for vimperator
*/

var FirebugPentadactyl = function () {
  var fb = Firebug,
  fbCommandLine = fb.CommandLine;
  return {
    _initialize: function () {
      var self = this;
      if (!this._initialized) {
        document.getElementById('fbCommandEditor').addEventListener('blur', function (event) {
          self.console_run();
        },
        true);
        this._initialized = true;
      }
      return this;
    },
    _exec: function (args, query) {
      var self = this;
      args.forEach(function (cmd) {
        self[cmd.replace('-', '_')](query);
      });
    },
    _initialized: false,
    open: function () {
      if (!fb.chrome.isOpen()) {
        fb.toggleBar(true, 'console');
      }
      setTimeout(function () {
        var browser = fb.chrome.getCurrentBrowser();
        browser.chrome.getSelectedPanel().document.defaultView.focus();
      },
      100);
    },
    off: function () {
      fb.closeFirebug(true);
    },
    close: function () {
      if (fb.chrome.isOpen()) {
        fb.toggleBar();
      }
    },
    toggle: function () {
      fb.toggleBar();
    },
    previous_tab: function () {
      fb.chrome.gotoPreviousTab();
    },
    next: function (query) {
      var tab = query[0]['-tab'] || query[0]['-T'];
      return (tab ? fb.chrome.switchToPanel(fb.currentContext, tab) : fb.chrome.gotoSiblingTab(!tab));
    },
    console_run: function () {
      if (fb.chrome.isOpen()) {
        fbCommandLine.enter(fb.currentContext);
      }
    },
    console_clear: function () {
      if (fb.chrome.isOpen()) {
        fb.Console.clear();
      }
    },
    console_focus: function () {
      if (!fb.chrome.isOpen()) {
        this.open();
      }
      fb.chrome.switchToPanel(fb.currentContext, "console");
      var cmLine = fbCommandLine.getSingleRowCommandLine(),
      cmEditor = fbCommandLine.getCommandEditor(),
      commandLine = fb.commandEditor ? cmEditor: cmLine;

      setTimeout(function () {
        commandLine.select();
      },
      100);
    }
  };
};

var fbp = (new FirebugPentadactyl())._initialize();

group.commands.add(['firebug', 'fbp'], 'Control firebug from within pentadactyl.', function (args) {
  fbp._exec(args, arguments);
},
{
  count: true,
  argCount: '*',
  completer: function (context) {
    var cmds = [],
    cmd;
    for (cmd in fbp) {
      if (fbp.hasOwnProperty(cmd) && cmd.indexOf('_') !== 0) {
        cmds.push([cmd.replace('_', '-'), '']);
      }
    }
    context.completions = cmds;
  },
  options: [{
    names: ["-tab", "-T"],
    description: "Select Tab",
    type: commands.OPTION_STRING,
    completer: function (context) {
      var array = ['console', 'html', 'dom', 'css', 'net', 'script'];
      context.completions = array.map(function (item) {
        return [item, ''];
      });
    }
  }]
});
