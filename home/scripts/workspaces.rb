#!/usr/bin/env ruby

# xprop cannot watch urgency events on all windows.
# i haven't figured out how to watch a proc in crystal
# therefore we need a spy proc per window
# trying ruby.  (py is next best choice)
# not sure if ruby is fast enough, but keeping it all in proc without bash pipes may help

# todo: wmctrl's needs_attention works
# termite setting the urgency WM_HINT goes unnoticed.
# figure out what property triggers it.
require "xlib-objects"
require "pry"

class Event
  EVENT_MAP = %i[
    KeyPress KeyRelease ButtonPress ButtonRelease MotionNotify EnterNotify
    LeaveNotify FocusIn FocusOut KeymapNotify Expose GraphicsExpose NoExpose
    VisibilityNotify CreateNotify DestroyNotify UnmapNotify MapNotify MapRequest
    ReparentNotify ConfigureNotify ConfigureRequest GravityNotify ResizeRequest
    CirculateNotify CirculateRequest PropertyNotify SelectionClear
    SelectionRequest SelectionNotify ColormapNotify ClientMessage MappingNotify
    GenericEvent LASTEvent
  ].each_with_object({}) { |constant, hash| hash[Xlib.const_get(constant)] = constant  }

  def initialize(xlib_event)
    @attributes = xlib_event.members.zip(xlib_event.values).to_h
  end

  def update?
    PropertyNotify? || ClientMessage?
  end

  private

  EVENT_MAP.each do |event_value, event_name|
    define_method("#{event_name}?") do
      @attributes[:type] == event_value
    end
  end
end

class Formatter
  def initialize(cfg)
    @cfg = cfg
  end

  def workspaces(workspaces_hash)
    workspaces_hash.map do |id, workspace|
      workspace(id, workspace)
    end.join(@cfg.delimiter)
  end

  private

  def workspace(id, workspace)
    pre = "pre_#{workspace.state}"
    post = "post_#{workspace.state}"
    name = @cfg.ws_name(id) || workspace.name
    [@cfg.send(pre), name, @cfg.send(post)].join
  end
end

class Cli
  def initialize(cfg)
    @display = XlibObj::Display.new(":0")
    @root = Root.new(@display)
    @formatter = Formatter.new(cfg)
  end

  def main
    loop do
      e = Xlib::X.next_event(@display)
      update if Event.new(e).update?
    end
  end

  def update
    all = Array(@root.all_windows).each_with_object(@root.workspaces) do |window, workspaces|
      ws = workspaces[window.workspace]

      if window.urgent?
        ws.urgent!
      else
        ws.occupied!
      end
      # case
      # when window.urgent? then ws.urgent!
      # when
      # end
      # next if ws.urgent?
      # ws.urgent = window.urgent?

      # next if ws.visible? || ws.occupied?
      # ws.occupied = true
    end

    # binding.pry

    puts @formatter.workspaces(all)
  end
end

class Workspace
  attr_writer :urgent, :visible, :occupied
  attr_reader :name, :state

  URGENT = :urgent
  VISIBLE = :visible
  OCCUPIED = :occupied
  EMPTY = :empty

  def initialize(name, visible)
    @name = name
    @state = :empty
    visible! if visible
  end

  def urgent!
    @state = URGENT
  end

  def visible!
    @state = VISIBLE unless @state == URGENT
  end

  def occupied!
    @state = OCCUPIED if @state == EMPTY
  end
end

class Root
  NET_CLIENT_LIST_STACKING=:_NET_CLIENT_LIST_STACKING

  def initialize(display)
    @root = display.screens.first.root_window
    @display = display
    init_events!
  end

  def all_windows
    Array(@root.property(NET_CLIENT_LIST_STACKING)).map do |xlibobj_window|
      Window.new @display, xlibobj_window
    end
  end

  def workspaces
    names = XlibObj::Window::Property.new(@root, :_NET_DESKTOP_NAMES).get
    current_ws = XlibObj::Window::Property.new(@root, :_NET_CURRENT_DESKTOP).get.first
    names.each_with_object({}).with_index do |(name, workspaces), index|
      workspaces[index] = Workspace.new(name, current_ws == index)
    end
  end

  private

  def init_events!
    @root.on(:property_change, :property_notify) { | event| puts event }
    @root.on(:substructure_notify, :client_message) { | event| puts event }
#     XlibObj::Extension::Core::Event::MASKS.keys
#     XlibObj::Extension::Core::Event::TYPES.keys
  end
end

class Window
  NET_WM_NAME = :_NET_WM_NAME
  NET_WM_DESKTOP = :_NET_WM_DESKTOP
  NET_WM_STATE = :_NET_WM_STATE

  NET_WM_STATE_DEMANDS_ATTENTION = :_NET_WM_STATE_DEMANDS_ATTENTION
  def initialize(display, xlibobj_window)
    @display = display
    @window = xlibobj_window
  end

  def id
    @window.id
  end

  def name
    @window.property(NET_WM_NAME)&.join
  end

  def workspace
    @workspace ||= @window.property(NET_WM_DESKTOP).first
  end

  def focused?
    @window.focused?
  end

  def urgent?
    states = @window.property(NET_WM_STATE)
    states&.any? { |state| state.name == NET_WM_STATE_DEMANDS_ATTENTION }
  end
end

class Configuration
  attr_reader :delimiter, :pre_urgent, :post_urgent, :pre_visible,
    :post_visible, :pre_occupied, :post_occupied, :pre_empty, :post_empty

  def initialize
    @delimiter = " "
    @workspaces = Array.new(10, &:itself).zip(Array.new(10).fill("⚫")).to_h # hash of ws id => name

    @pre_urgent = "%{F#ff5500}"
    @post_urgent = "%{F-}"

    @pre_visible = "%{F#ffffff}"
    @post_visible = "%{F-}"

    @pre_occupied = "%{F#777777}"
    @post_occupied = "%{F-}"

    @pre_empty = "%{F#333333}"
    @post_empty = "%{F-}"
  end

  def ws_name(ws)
    @workspaces[ws]
  end
end

cfg = Configuration.new
Cli.new(cfg).main
