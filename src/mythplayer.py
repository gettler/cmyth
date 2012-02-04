#!/usr/bin/python
#
# An example of using the libcmyth Python bindings
#
# Inspired by the following example of wxPython and MplayerCtrl:
#
#     http://www.blog.pythonlibrary.org/2010/07/24/wxpython-creating-a-simple-media-player/
#

import sys
import os
import string
import wx
import MplayerCtrl as mpc
import cmyth
import threading
import socket
import random
import SocketServer
import SimpleHTTPServer
import getopt

from os import pathsep

server = ''
mplayer_port = 0

current = None
frame = None

class Frame(wx.Frame):
    class preferenes(wx.Dialog):
        def __init__(self, parent, id, title):
            global server

            wx.Dialog.__init__(self, parent, id, title, size=(400,200))
            main = wx.BoxSizer(wx.VERTICAL)
            one = wx.BoxSizer(wx.HORIZONTAL)
            two = wx.BoxSizer(wx.HORIZONTAL)

            label = wx.StaticText(self, label='Please configure mythplayer:')
            one.Add(label, 1, wx.ALIGN_CENTER|wx.EXPAND|wx.ALL, 20)

            label = wx.StaticText(self, label='Server:')
            self.entry = wx.TextCtrl(self, value=server, size=(200,-1))
            two.Add(label, 1, wx.ALL, 5)
            two.Add(self.entry, 0, wx.ALL, 5)

            done = wx.Button(self, label = 'Done')
            done.Bind(wx.EVT_BUTTON, self.on_done)

            main.Add(one, 1, wx.ALL, 5)
            main.Add(two, 1, wx.ALL, 5)
            main.Add(done, 1, wx.ALIGN_RIGHT|wx.ALL, 5)

            self.SetSizer(main)

        def on_done(self, event):
            global frame
            new_server = self.entry.GetValue()
            new_server = new_server.encode('ascii','ignore')
            self.Close()
            frame.init_server(new_server)

    def __init__(self, parent, id, title, mplayer):
        global server

        wx.Frame.__init__(self, parent, id, title, size=(900,600))
        self.panel = wx.Panel(self)

        self.selected = None
        self.playing = False
        
        # Create the GUI
        self.create_menu()
        self.create_widgets()

        # Bind the mplayer events
        self.Bind(mpc.EVT_MEDIA_STARTED, self.on_media_started)
        self.Bind(mpc.EVT_MEDIA_FINISHED, self.on_media_started)
        self.Bind(mpc.EVT_PROCESS_STARTED, self.on_process_started)
        self.Bind(mpc.EVT_PROCESS_STOPPED, self.on_process_stopped)

        # Get the info about the recordings from the MythTV backend
        try:
            self.conn = cmyth.connection(server)
            self.programs = self.conn.get_proglist()
            self.create_title_list()
        except:
            print 'connection failed'

        random.seed()
        self.server = media()
        self.server.daemon = True
        self.server.start()

    def init_server(self, new_server):
        global server
        if new_server != server:
            print 'switch from %s to %s' % (server, new_server)
            server = new_server
            self.title_box.Set([])
            self.episode_box.Set([])
            try:
                self.conn = cmyth.connection(server)
                self.programs = self.conn.get_proglist()
                self.create_title_list()
            except:
                print 'connection failed'

    def create_menu(self):
        menubar = wx.MenuBar()
        file_menu = wx.Menu()
        open_item = file_menu.Append(wx.NewId(), "&Open File", "Open File")
        prefs_item = file_menu.Append(wx.NewId(), "&Preferences", "Change Preferences")
        exit_item = file_menu.Append(wx.NewId(), "&Exit", "Exit")
        menubar.Append(file_menu, '&File')
        help_menu = wx.Menu()
        about_item = help_menu.Append(wx.NewId(), "&About", "About")
        menubar.Append(help_menu, '&Help')
 
        self.SetMenuBar(menubar)

        self.Bind(wx.EVT_MENU, self.on_exit, exit_item)
        self.Bind(wx.EVT_MENU, self.on_open, open_item)
        self.Bind(wx.EVT_MENU, self.on_about, about_item)
        self.Bind(wx.EVT_MENU, self.on_prefs, prefs_item)

    def create_widgets(self):
        main = wx.BoxSizer(wx.VERTICAL)
        info = wx.BoxSizer(wx.HORIZONTAL)
        buttons = wx.BoxSizer(wx.VERTICAL)
        player = wx.BoxSizer(wx.HORIZONTAL)

        self.title_box = wx.ListBox(self.panel, size=(300,200),
                                    choices=[], style=wx.LB_SINGLE)
        self.title_box.Bind(wx.EVT_LISTBOX, self.title_selected)
        self.episode_box = wx.ListBox(self.panel, size=(300,200),
                                      choices=[], style=wx.LB_SINGLE)
        self.episode_box.Bind(wx.EVT_LISTBOX, self.episode_selected)

        info.Add(self.title_box, proportion=0, flag=wx.RIGHT, border=5)
        info.Add(self.episode_box, proportion=0)

        play = wx.Button(self.panel, label = 'play')
        pause = wx.Button(self.panel, label = 'pause')
        stop = wx.Button(self.panel, label = 'stop')

        buttons.Add(play, flag=wx.LEFT|wx.RIGHT|wx.TOP, border=5)
        buttons.Add(pause, flag=wx.LEFT|wx.RIGHT|wx.TOP, border=5)
        buttons.Add(stop, flag=wx.LEFT|wx.RIGHT|wx.TOP, border=5)

        play.Bind(wx.EVT_BUTTON, self.play)
        pause.Bind(wx.EVT_BUTTON, self.pause)
        stop.Bind(wx.EVT_BUTTON, self.stop)

        self.mplayer = mpc.MplayerCtrl(self.panel, -1, mplayer)

        player.Add(buttons, 0, wx.ALL|wx.EXPAND, 5)
        player.Add(self.mplayer, 1, wx.ALL|wx.EXPAND, 5)

        main.Add(info, flag=wx.EXPAND|wx.LEFT|wx.RIGHT|wx.TOP, border=10)
        main.Add(player, 1, wx.EXPAND)

        self.panel.SetSizer(main)

        self.Show()

    def create_title_list(self):
        self.titles = []
        subtitles = []
        count = self.programs.get_count()
        for i in range(count):
            prog = self.programs.get_prog(i)
            t = str(prog.title())
            if not t in self.titles:
                self.titles.append(t)
        self.title_box.Set(self.titles)

    def on_exit(self, event):
        self.mplayer.Quit()
        sys.exit(0)

    def on_open(self, event):
        dialog = wx.FileDialog(self, message='Choose a file',
                               defaultDir=os.getcwd(),
                               defaultFile='',
                               wildcard='*.*',
                               style=wx.OPEN | wx.CHANGE_DIR
                               )
        if dialog.ShowModal() == wx.ID_OK:
            path = dialog.GetPath()
            self.currentFolder = os.path.dirname(path[0])
            trackPath = '"%s"' % path.replace("\\", "/")
            self.mplayer.Loadfile(trackPath)
            self.playing = True

    def on_about(self, event):
        dialog = wx.MessageDialog(self,
                                  'An example MythTV frontend using libcmyth.',
                                  'mythplayer', wx.OK|wx.ICON_INFORMATION)
        dialog.ShowModal()
        dialog.Destroy()

    def on_prefs(self, event):
        dialog = self.preferenes(self, -1, 'buttons')
        dialog.ShowModal()
        dialog.Destroy()

    def title_selected(self, event):
        self.episodes = []
        subtitles = []
        index = event.GetSelection()
        count = self.programs.get_count()
        for i in range(count):
            prog = self.programs.get_prog(i)
            t = str(prog.title())
            if t == self.titles[index]:
                subtitles.append(str(prog.subtitle()))
                self.episodes.append(prog)
        self.episode_box.Set(subtitles)

    def episode_selected(self, event):
        global mplayer_port
        index = event.GetSelection()
        self.selected = self.episodes[index]

    def on_media_started(self, event):
        print 'media started'

    def on_media_finished(self, event):
        print 'media finished'

    def on_process_started(self, event):
        print 'process started'

    def on_process_stopped(self, event):
        print 'process stopped'

    def play(self, event):
        if self.playing == True:
            return
        if self.selected != None:
            self.playing = True
            url = 'http://localhost:%d/%s' % (mplayer_port,
                                              self.selected.pathname())
            print 'open %s' % url
            self.server.stream(self.selected)
            self.mplayer.Loadfile(url)

    def pause(self, event):
        if self.playing == True:
            self.mplayer.Pause()

    def stop(self, event):
        if self.playing == True:
            self.mplayer.Stop()
            self.playing = False

class media(threading.Thread):
    def run(self):
        global mplayer_port
        while True:
            mplayer_port = random.randint(6000, 9000)
            self.s = SocketServer.ThreadingTCPServer(('localhost', mplayer_port),
                                                     httpd)
            if self.s != None:
                print 'httpd server started on port %d' % mplayer_port
                break
        self.s.serve_forever()

    def stream(self, prog):
        global current
        current = prog

class httpd(SimpleHTTPServer.SimpleHTTPRequestHandler):
    def do_GET(self):
        block = 1024 * 128
        print 'GET %s' % self.path
        global current
        self.prog = current
        length = self.prog.length()
        file = self.prog.open()
        self.send_response(200)
        self.send_header('Content-Length', str(length))
        self.end_headers()
        file.seek(0)
        while True:
            rc,buf = file.read()
            if rc < 0:
                break
            self.wfile.write(buf)

    def write_file(self, s):
        block = 1024 * 128
        server = self.prog.host()
        file = self.prog.open(server)
        length = self.prog.length()
        name = self.prog.pathname()[1:]
        print 'Writing %d bytes of %s' % (length, name)
        header = 'HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n' % length
        print header
        s.send(header)
        file.seek(0)
        while True:
            rc,buf = file.read()
            if rc < 0:
                break
            s.send(buf)
        s.close()

def find_binary(filename):
    '''Find a file in the system search path
    ''' 
    path = os.environ['PATH']
    paths = string.split(path, pathsep)
    for i in paths:
        name = os.path.join(i, filename)
        if os.path.isfile(name):
            return name
    return ''
    
def usage(code):
    print 'Usage: mythplayer [options]'
    print '       --help              print this help'
    print '       --server hostname   MythTV server hostname/IP'
    sys.exit(code)

try:
    opts, args = getopt.getopt(sys.argv[1:], 'hs:',
                               [ 'help', 'server=' ])
except getopt.GetoptError:
    usage(1)

for o, a in opts:
    if o in ('-h', '--help'):
        usage(0)
    if o in ('-s', '--server'):
        server = a

app = wx.App(redirect=False)
mplayer = find_binary('mplayer')
frame = Frame(None, -1, 'mythplayer', mplayer)
app.MainLoop()
