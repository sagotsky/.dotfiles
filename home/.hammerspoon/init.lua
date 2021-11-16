-- mod is my mod key
local mod = {'cmd', 'alt', 'ctrl', 'shift' }

-- mod 0-9 jumps to app
apps = {
  {'1', 'Firefox'},
  {'2', 'Spotify'},
  {'4', 'iTerm'},
  {'5', 'Slack'},
  {'6', 'Messages'},
  {'0', 'Zoom'},
}
for i, app in ipairs(apps) do
  hs.hotkey.bind(mod, app[1], function()
    hs.application.launchOrFocus(app[2])
  end)
end

-- mod+f  fullscreens current window
hs.hotkey.bind(mod, 'F', function()
  win = hs.window.frontmostWindow()
  if win == nil then
    hs.alert.show("Can't access focused window.  Does hammerspoon have accessibility privs?")
  else
    win:maximize()
  end
end)


-- auto reload this file
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
