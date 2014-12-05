#!/bin/sh

while read -a line ; do
  num=${line[0]}
  name="${line[1]}"
  hex="${line[2]}"

  echo -n -e "[38;5;${num}m${num}:${name}[39m "
done <<EOF
0            SYSTEM            #000000
1            SYSTEM            #800000
2            SYSTEM            #008000
3            SYSTEM            #808000
4            SYSTEM            #000080
5            SYSTEM            #800080
6            SYSTEM            #008080
7            SYSTEM            #c0c0c0
8            SYSTEM            #808080
9            SYSTEM            #ff0000
10           SYSTEM            #00ff00
11           SYSTEM            #ffff00
12           SYSTEM            #0000ff
13           SYSTEM            #ff00ff
14           SYSTEM            #00ffff
15           SYSTEM            #ffffff
16           Grey0             #000000
17           NavyBlue          #00005f
18           DarkBlue          #000087
19           Blue3             #0000af
20           Blue3             #0000d7
21           Blue1             #0000ff
22           DarkGreen         #005f00
23           DeepSkyBlue4      #005f5f
24           DeepSkyBlue4      #005f87
25           DeepSkyBlue4      #005faf
26           DodgerBlue3       #005fd7
27           DodgerBlue2       #005fff
28           Green4            #008700
29           SpringGreen4      #00875f
30           Turquoise4        #008787
31           DeepSkyBlue3      #0087af
32           DeepSkyBlue3      #0087d7
33           DodgerBlue1       #0087ff
34           Green3            #00af00
35           SpringGreen3      #00af5f
36           DarkCyan          #00af87
37           LightSeaGreen     #00afaf
38           DeepSkyBlue2      #00afd7
39           DeepSkyBlue1      #00afff
40           Green3            #00d700
41           SpringGreen3      #00d75f
42           SpringGreen2      #00d787
43           Cyan3             #00d7af
44           DarkTurquoise     #00d7d7
45           Turquoise2        #00d7ff
46           Green1            #00ff00
47           SpringGreen2      #00ff5f
48           SpringGreen1      #00ff87
49           MediumSpringGreen #00ffaf
50           Cyan2             #00ffd7
51           Cyan1             #00ffff
52           DarkRed           #5f0000
53           DeepPink4         #5f005f
54           Purple4           #5f0087
55           Purple4           #5f00af
56           Purple3           #5f00d7
57           BlueViolet        #5f00ff
58           Orange4           #5f5f00
59           Grey37            #5f5f5f
60           MediumPurple4     #5f5f87
61           SlateBlue3        #5f5faf
62           SlateBlue3        #5f5fd7
63           RoyalBlue1        #5f5fff
64           Chartreuse4       #5f8700
65           DarkSeaGreen4     #5f875f
66           PaleTurquoise4    #5f8787
67           SteelBlue         #5f87af
68           SteelBlue3        #5f87d7
69           CornflowerBlue    #5f87ff
70           Chartreuse3       #5faf00
71           DarkSeaGreen4     #5faf5f
72           CadetBlue         #5faf87
73           CadetBlue         #5fafaf
74           SkyBlue3          #5fafd7
75           SteelBlue1        #5fafff
76           Chartreuse3       #5fd700
77           PaleGreen3        #5fd75f
78           SeaGreen3         #5fd787
79           Aquamarine3       #5fd7af
80           MediumTurquoise   #5fd7d7
81           SteelBlue1        #5fd7ff
82           Chartreuse2       #5fff00
83           SeaGreen2         #5fff5f
84           SeaGreen1         #5fff87
85           SeaGreen1         #5fffaf
86           Aquamarine1       #5fffd7
87           DarkSlateGray2    #5fffff
88           DarkRed           #870000
89           DeepPink4         #87005f
90           DarkMagenta       #870087
91           DarkMagenta       #8700af
92           DarkViolet        #8700d7
93           Purple            #8700ff
94           Orange4           #875f00
95           LightPink4        #875f5f
96           Plum4             #875f87
97           MediumPurple3     #875faf
98           MediumPurple3     #875fd7
99           SlateBlue1        #875fff
100          Yellow4           #878700
101          Wheat4            #87875f
102          Grey53            #878787
103          LightSlateGrey    #8787af
104          MediumPurple      #8787d7
105          LightSlateBlue    #8787ff
106          Yellow4           #87af00
107          DarkOliveGreen3   #87af5f
108          DarkSeaGreen      #87af87
109          LightSkyBlue3     #87afaf
110          LightSkyBlue3     #87afd7
111          SkyBlue2          #87afff
112          Chartreuse2       #87d700
113          DarkOliveGreen3   #87d75f
114          PaleGreen3        #87d787
115          DarkSeaGreen3     #87d7af
116          DarkSlateGray3    #87d7d7
117          SkyBlue1          #87d7ff
118          Chartreuse1       #87ff00
119          LightGreen        #87ff5f
120          LightGreen        #87ff87
121          PaleGreen1        #87ffaf
122          Aquamarine1       #87ffd7
123          DarkSlateGray1    #87ffff
124          Red3              #af0000
125          DeepPink4         #af005f
126          MediumVioletRed   #af0087
127          Magenta3          #af00af
128          DarkViolet        #af00d7
129          Purple            #af00ff
130          DarkOrange3       #af5f00
131          IndianRed         #af5f5f
132          HotPink3          #af5f87
133          MediumOrchid3     #af5faf
134          MediumOrchid      #af5fd7
135          MediumPurple2     #af5fff
136          DarkGoldenrod     #af8700
137          LightSalmon3      #af875f
138          RosyBrown         #af8787
139          Grey63            #af87af
140          MediumPurple2     #af87d7
141          MediumPurple1     #af87ff
142          Gold3             #afaf00
143          DarkKhaki         #afaf5f
144          NavajoWhite3      #afaf87
145          Grey69            #afafaf
146          LightSteelBlue3   #afafd7
147          LightSteelBlue    #afafff
148          Yellow3           #afd700
149          DarkOliveGreen3   #afd75f
150          DarkSeaGreen3     #afd787
151          DarkSeaGreen2     #afd7af
152          LightCyan3        #afd7d7
153          LightSkyBlue1     #afd7ff
154          GreenYellow       #afff00
155          DarkOliveGreen2   #afff5f
156          PaleGreen1        #afff87
157          DarkSeaGreen2     #afffaf
158          DarkSeaGreen1     #afffd7
159          PaleTurquoise1    #afffff
160          Red3              #d70000
161          DeepPink3         #d7005f
162          DeepPink3         #d70087
163          Magenta3          #d700af
164          Magenta3          #d700d7
165          Magenta2          #d700ff
166          DarkOrange3       #d75f00
167          IndianRed         #d75f5f
168          HotPink3          #d75f87
169          HotPink2          #d75faf
170          Orchid            #d75fd7
171          MediumOrchid1     #d75fff
172          Orange3           #d78700
173          LightSalmon3      #d7875f
174          LightPink3        #d78787
175          Pink3             #d787af
176          Plum3             #d787d7
177          Violet            #d787ff
178          Gold3             #d7af00
179          LightGoldenrod3   #d7af5f
180          Tan               #d7af87
181          MistyRose3        #d7afaf
182          Thistle3          #d7afd7
183          Plum2             #d7afff
184          Yellow3           #d7d700
185          Khaki3            #d7d75f
186          LightGoldenrod2   #d7d787
187          LightYellow3      #d7d7af
188          Grey84            #d7d7d7
189          LightSteelBlue1   #d7d7ff
190          Yellow2           #d7ff00
191          DarkOliveGreen1   #d7ff5f
192          DarkOliveGreen1   #d7ff87
193          DarkSeaGreen1     #d7ffaf
194          Honeydew2         #d7ffd7
195          LightCyan1        #d7ffff
196          Red1              #ff0000
197          DeepPink2         #ff005f
198          DeepPink1         #ff0087
199          DeepPink1         #ff00af
200          Magenta2          #ff00d7
201          Magenta1          #ff00ff
202          OrangeRed1        #ff5f00
203          IndianRed1        #ff5f5f
204          IndianRed1        #ff5f87
205          HotPink           #ff5faf
206          HotPink           #ff5fd7
207          MediumOrchid1     #ff5fff
208          DarkOrange        #ff8700
209          Salmon1           #ff875f
210          LightCoral        #ff8787
211          PaleVioletRed1    #ff87af
212          Orchid2           #ff87d7
213          Orchid1           #ff87ff
214          Orange1           #ffaf00
215          SandyBrown        #ffaf5f
216          LightSalmon1      #ffaf87
217          LightPink1        #ffafaf
218          Pink1             #ffafd7
219          Plum1             #ffafff
220          Gold1             #ffd700
221          LightGoldenrod2   #ffd75f
222          LightGoldenrod2   #ffd787
223          NavajoWhite1      #ffd7af
224          MistyRose1        #ffd7d7
225          Thistle1          #ffd7ff
226          Yellow1           #ffff00
227          LightGoldenrod1   #ffff5f
228          Khaki1            #ffff87
229          Wheat1            #ffffaf
230          Cornsilk1         #ffffd7
231          Grey100           #ffffff
232          Grey3             #080808
233          Grey7             #121212
234          Grey11            #1c1c1c
235          Grey15            #262626
236          Grey19            #303030
237          Grey23            #3a3a3a
238          Grey27            #444444
239          Grey30            #4e4e4e
240          Grey35            #585858
241          Grey39            #626262
242          Grey42            #6c6c6c
243          Grey46            #767676
244          Grey50            #808080
245          Grey54            #8a8a8a
246          Grey58            #949494
247          Grey62            #9e9e9e
248          Grey66            #a8a8a8
249          Grey70            #b2b2b2
250          Grey74            #bcbcbc
251          Grey78            #c6c6c6
252          Grey82            #d0d0d0
253          Grey85            #dadada
254          Grey89            #e4e4e4
255          Grey93            #eeeeee
EOF
