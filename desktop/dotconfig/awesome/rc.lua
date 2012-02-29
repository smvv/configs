-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Widget library
require("vicious")

-- requires dev-lua/oocairo
require("blingbling")

-- Load Debian menu entries
-- require("debian.menu")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
-- beautiful.init("/usr/share/awesome/themes/zenburn/theme.lua")
beautiful.init("/home/".. os.getenv("USER")
                .. "/.config/awesome/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
    names = {"1", "2", "3", "4", "5", "6", "7", "8", "9"},
    layouts = {
        awful.layout.suit.max,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.left,
        awful.layout.suit.tile.left,
        awful.layout.suit.max
    }
}

-- for s = 1, screen.count() do
--     -- Each screen has its own tag table.
--     --
--     tags[s] = awful.tag(tags.names, s, tags.layout)
--     -- Start displaying tag #5 (5:dev)
--     tags[s][4].selected = true
-- end

for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = {}
    for tagnumber = 1, #tags.names do
        -- Add tags and name them.
        tags[s][tagnumber] = tag({name=tags.names[tagnumber],
                layout=tags.layouts[tagnumber]})
        -- tag(tagnames[tagnumber])
        -- Add tags to screen one by one.
        tags[s][tagnumber].screen = s
        awful.layout.set(tags.layouts[tagnumber], tags[s][tagnumber])
    end
     -- Start displaying tag #5 (5:dev)
     tags[s][4].selected = true
end

-- }}}

-- {{{ Menu
---- Create a laucher widget and a main menu
--myawesomemenu = {
--    { "manual", terminal .. " -e man awesome" },
--    { "edit config", editor_cmd .. " " .. awful.util.getdir("config")
--         .. "/rc.lua" },
--    { "restart", awesome.restart },
--    { "quit", awesome.quit },
--    { "shutdown", 'gksudo poweroff' },
--}
--
---- debian.menu.Debian_menu.Debian = tunion(debian.menu.Debian_menu.Debian,
----                                         {{"awesome", myawesomemenu},
----                                          {"open terminal", terminal}})
--
--mymainmenu = awful.menu({items = {
--    {"awesome", myawesomemenu, beautiful.awesome_icon},
--    -- {"Debian", debian.menu.Debian_menu.Debian},
--    {"open terminal", terminal}
--}})

-- mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
--                                      menu = mymainmenu })
-- }}}

-- {{{ Wibox

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
    awful.button({ }, 1, function (c)
        if not c:isvisible() then
            awful.tag.viewonly(c:tags()[1])
        end
        client.focus = c
        c:raise()
    end),
    awful.button({ }, 3, function ()
        if instance then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ width=250 })
        end
    end),
    awful.button({ }, 4, function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.button({ }, 5, function ()
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end))

-- Create a textclock widget
--mytextclock = awful.widget.textclock({align = "right"})
datewidget = widget({ type = "textbox" })
vicious.register(datewidget, vicious.widgets.date, "%b %d, %R")

-- Calendar widget to attach to the textclock
require('calendar2')
calendar2.addCalendarToWidget(datewidget)

---- Volume box widget
--volwidget = widget({type = "textbox"})
--vicious.register(volwidget, vicious.widgets.volume, " $1 $2 ", 5, 'Master')

--volbox = widget({ type = "textbox", name = "volbox", align = "right" })
--vicious.register(volbox, vicious.widgets.volbox, function (widget, args)
--    --local f = io.popen('mpc|grep volume' ..
--    --        '|awk {\'print \"   V: \[ \" $2 \"\" \] \'}')
--    local f = io.popen('amixer get Master | grep "%" | cut -d"[" -f2' ..
--                       '| sed "s/].*//g" | uniq | tr "\n" " "')
--    local v = f:read('*all') .. "   "
--    f:close()
--    return v
--end, 5)

--batwidget = awful.widget.progressbar()
--batwidget:set_width(4)
--batwidget:set_height(20)
--batwidget:set_vertical(true)
--batwidget:set_background_color("#494B4F")
--batwidget:set_border_color(nil)
--batwidget:set_color("#AECF96")
--batwidget:set_gradient_colors({ "#AECF96", "#88A175", "#FF5656" })
--vicious.register(batwidget, vicious.widgets.bat, "$2", 61, "BAT0")

--cpuwidget = widget({type = "textbox"})
--vicious.register(cpuwidget, vicious.widgets.cpu, " | $1% ")

---- CPU usage widget
--cpuwidget = awful.widget.graph()
--cpuwidget:set_width(50)
--cpuwidget:set_background_color("#494B4F")
--cpuwidget:set_color("#88A175")
----cpuwidget:set_gradient_colors({ "#FF5656", "#88A175", "#AECF96" })
--
---- Register widget
--vicious.register(cpuwidget, vicious.widgets.cpu, "$1")

-- CPU usage graph
cpugraph=blingbling.classical_graph.new()
--cpugraph:set_height(18)
cpugraph:set_width(60)
cpugraph:set_tiles_color("#33333366")
cpugraph:set_graph_color("#00000000") -- hide graph body
--bind top popup on the graph
blingbling.popups.htop(cpugraph.widget,
       { title_color=beautiful.notify_font_color_1,
          user_color=beautiful.notify_font_color_2,
          root_color=beautiful.notify_font_color_3,
          terminal="urxvt"})
vicious.register(cpugraph, vicious.widgets.cpu, '$1', 2)

-- mem usage graph
memwidget=blingbling.classical_graph.new()
memwidget:set_width(60)
memwidget:set_tiles_color("#33333366")
memwidget:set_graph_line_color("#aaaacccc")
memwidget:set_filled_color("#00000000")
memwidget:set_graph_color("#00000000")
--bind top popup on the graph
vicious.register(memwidget, vicious.widgets.mem, '$1', 2)

-- Volume widget
volumecfg = {}
volumecfg.cardid  = 0
volumecfg.channel = "Master"
volumecfg.widget = widget({ type = "textbox", name = "volumecfg.widget", align = "right" })

volumecfg_t = awful.tooltip({ objects = { volumecfg.widget },})
volumecfg_t:set_text("Volume")

-- command must start with a space!
volumecfg.mixercommand = function (command)
       local fd = io.popen("amixer -c " .. volumecfg.cardid .. command)
       local status = fd:read("*all")
       fd:close()

       local volume = string.match(status, "(%d?%d?%d)%%")
       volume = string.format("% 3d", volume)
       status = string.match(status, "%[(o[^%]]*)%]")
       if string.find(status, "on", 1, true) then
               volume = volume .. "%"
       else
               volume = volume .. "M"
       end
       volumecfg.widget.text = volume
end
volumecfg.update = function ()
       volumecfg.mixercommand(" sget " .. volumecfg.channel)
end
volumecfg.up = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 1%+")
end
volumecfg.down = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 1%-")
end
volumecfg.toggle = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " toggle")
end
volumecfg.widget:buttons({
       awful.button({ }, 4, function () volumecfg.up() end),
       awful.button({ }, 5, function () volumecfg.down() end),
       awful.button({ }, 1, function () volumecfg.toggle() end)
})
volumecfg.update()

---- RAM usage widget
--memwidget = awful.widget.graph()
--memwidget:set_width(50)
--memwidget:set_background_color("#494B4F")
--memwidget:set_color("#9999cc")
----cpuwidget:set_gradient_colors({ "#FF5656", "#88A175", "#AECF96" })
--
---- RAM usage tooltip
--memwidget_t = awful.tooltip({ objects = { memwidget.widget },})
--
--vicious.cache(vicious.widgets.mem)
--vicious.register(memwidget, vicious.widgets.mem,
--                function (widget, args)
--                    memwidget_t:set_text(" RAM: " .. args[2] .. "MB / " .. args[3] .. "MB ")
--                    return args[1]
--                 end, 1)

-- Create a gmail widget
gmailwidget = widget({ type = "textbox" })
vicious.register(gmailwidget, vicious.widgets.gmail, "<span color=\"red\">M: ${count}</span>", 10)

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout =
        awful.widget.layout.horizontal.leftright })

    -- Create an imagebox widget which will contains an icon indicating which
    -- layout we're using. We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
        awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
        awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
        awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))

    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all,
        mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
        return awful.widget.tasklist.label.currenttags(c, s)
    end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            layout = awful.widget.layout.horizontal.leftright,
            mylayoutbox[s],
            mytaglist[s],
            mypromptbox[s],
            gmailwidget,
            cpugraph,
            memwidget,
            s == 1 and mysystray or nil,
            volumecfg.widget,
            datewidget,
            mytasklist[s]
        },
        --mytextclock,
        --volwidget,
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    --awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

--  Key bindings {{{
globalkeys = awful.util.table.join(
    awful.key({ modkey,         }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,         }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,         }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,         }, "j",      function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,         }, "k",      function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    --awful.key({ modkey,         }, "w",      function ()
    --    mymainmenu:show({keygrabber=true})
    --end),

    -- Layout manipulation
    awful.key({modkey, "Shift"  }, "j",      function () awful.client.swap.byidx(  1)    end),
    awful.key({modkey, "Shift"  }, "k",      function () awful.client.swap.byidx( -1)    end),
    awful.key({modkey, "Control"}, "j",      function () awful.screen.focus_relative( 1) end),
    awful.key({modkey, "Control"}, "k",      function () awful.screen.focus_relative(-1) end),
    awful.key({modkey,          }, "u",      awful.client.urgent.jumpto),
    awful.key({modkey,          }, "Tab",    function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({modkey,         }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({modkey,"Control"}, "r",      awesome.restart),
    awful.key({modkey,"Shift"  }, "q",      awesome.quit),

    awful.key({modkey,         }, "l",      function () awful.tag.incmwfact( 0.05)    end),
    awful.key({modkey,         }, "h",      function () awful.tag.incmwfact(-0.05)    end),
    awful.key({modkey,"Shift"  }, "h",      function () awful.tag.incnmaster( 1)      end),
    awful.key({modkey,"Shift"  }, "l",      function () awful.tag.incnmaster(-1)      end),
    awful.key({modkey,"Control"}, "h",      function () awful.tag.incncol( 1)         end),
    awful.key({modkey,"Control"}, "l",      function () awful.tag.incncol(-1)         end),
    awful.key({modkey,         }, "space",  function () awful.layout.inc(layouts,  1) end),
    awful.key({modkey,"Shift"  }, "space",  function () awful.layout.inc(layouts, -1) end),

    -- Prompt
    -- awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),
    awful.key({ modkey         }, "r",      function ()
        -- If you want to change the font, use 'xfontsel' to pick a font.
        awful.util.spawn("dmenu_run -i -p 'Run:".. -- ' -fn 'global:FreeSans.ttf" ..
            "' -nb '" .. beautiful.bg_normal     ..
            "' -nf '" .. beautiful.fg_normal     ..
            "' -sb '" .. beautiful.bg_focus      ..
            "' -sf '" .. beautiful.fg_focus      .. "'")
        end),

    awful.key({modkey          }, "x",       function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

clientkeys = awful.util.table.join(
    awful.key({modkey,         }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({modkey,"Shift"  }, "c",      function (c) c:kill()                         end),
    awful.key({modkey,"Control"}, "space",  awful.client.floating.toggle                     ),
    awful.key({modkey,"Control"}, "Return", function (c) c:swap(awful.client.getmaster()) end),
    --awful.key({modkey,       }, "o",      awful.client.movetoscreen                        ),
    awful.key({modkey,"Shift"  }, "r",      function (c) c:redraw()                       end),
    awful.key({modkey,         }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({modkey,         }, "g",      function (c) awful.util.spawn("clip2goog")    end),
    awful.key({modkey,         }, "o",      function (c) awful.util.spawn("clip2firefox") end),
    awful.key({modkey,         }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({modkey,         }, "m",      function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
    end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
--

-- }}}

-- {{{ Rules: application screen/tag mapping
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     -- Remove the gap between two windows (since this is very
                     -- annoying when using multiple terminals).
                     size_hints_honor = false,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "gimp" }, properties = { floating = true } },
    { rule = { class = "chromium" }, properties = { tags = tags[1][2] } },
    { rule = { class = "liferea" }, properties = { tags = tags[1][2] } },
    { rule = { class = "Wine" }, properties = { floating = true } },
    { rule = { class = "QEMU" }, properties = { floating = true } },
    { rule = { class = "mplayer" }, properties = { floating = true } },

    -- Set Firefox to always map on tags number 3 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
    -- { rule = {
    --     name = "rhythmbox", -- name of the window. Its not case sensitive.
    --     tags = "fm",        -- name of the tag you want Rhythmbox to appear on.
    --     float = "false",    -- layout style. Keep maximized.
    --     screen = "0",       -- which monitor to display on.
    --     not_master = false  -- we want it to be the master window
    -- } }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    --c:add_signal("mouse::enter", function(c)
    --    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
    --        and awful.client.focus.filter(c) then
    --        client.focus = c
    --    end
    --end)

    -- Check if application is spawn on start up
    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Auto run applications
autorun = true
autorunApps =
{
    "xcompmgr",
    terminal,
    --"firefox",
    "liferea",
    "radiotray",
    "sh -c 'transset-df -i $(xwininfo -root -tree | grep \"(has no name): ()  1920x19+0+0\" | egrep -o \"0x[a-z0-9]{4,}\") 0.8'",
}
if autorun then
   for _, app in pairs(autorunApps) do
       awful.util.spawn(app)
   end
end
-- }}}

-- vim: foldmethod=marker
