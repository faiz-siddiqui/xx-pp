//@version=4
study("Camrilla Fibonachi Breakout","Cam-Fib-Bo",overlay=true)

NormalVwap=vwap(hlc3)
H = vwap(high)
L = vwap(low)
O = vwap(open)
C = vwap(close)
color G1 = color.green
color R1 = color.red
col2b = O > C ? R1 : G1

showvwap = input(title = "Show VWAP", defval = true)


plot(showvwap ? NormalVwap : na ,"VWAP", style=plot.style_line, linewidth=3, color=col2b)

showvwma = input(title = "Show VWMA", defval = true)
vwmaperiod = 14 // input(title="VWMA Period",type=input.integer,defval=14)

plot(showvwma   ? vwma(close,vwmaperiod) : na,   linewidth=4,color =  color.purple,title="VWMA")



showstyle = input(title="Indicator Line Plotting", defval="Default", options=["Default", "Continues Lines"])

START_TIME = timestamp(year, month, dayofmonth, 0, 0, 0)
DAILY_END_TIME = security(syminfo.tickerid, 'D', time_close, lookahead=barmerge.lookahead_on)



Atr=5 // input(defval=5,title="Atr Period",minval=1,maxval=500)
Hhv=10 //input(defval=10,title="HHV Period",minval=1,maxval=500)
Mult=2.5 // input(defval=2.5,title="Multiplier",minval=0.1)

Prev = highest(high-Mult*atr(Atr),Hhv),barssince(close>highest(high-Mult*atr(Atr),Hhv) and close>close[1])
TS = iff(cum(1)<16 ,close,iff( close > highest(high-Mult*atr(Atr),Hhv) and close>close[1],highest(high-Mult*atr(Atr),Hhv),Prev))

Color=iff(close>TS,color.green,iff(close<TS,color.red,color.black))
//plot(TS,title="ATR Trailing Stoploss",color=Color,style=plot.style_stepline, linewidth=2)

showtsl = input(title = "Show ATR Trailing Stoploss", defval = true)


plot(showtsl ? TS : na,title="ATR Trailing Stoploss",color=Color,style=plot.style_stepline, linewidth=1)



sess = input("0915-1015", type=input.session, title="Initial Balance Period") 
t = time(timeframe.period, sess + ":1234567")
hide = timeframe.isintraday  and timeframe.multiplier <= 10


is_newbar(res) => change(time(res)) != 0
in_session = not na(t)
is_first = in_session and not in_session[1]

orb_high = float(na)
orb_low = float(na)

if is_first
    orb_high := high
    orb_low := low
else
    orb_high := orb_high[1]
    orb_low := orb_low[1]
if high > orb_high and in_session
    orb_high := high
if low < orb_low and in_session
    orb_low := low

show15highlow = input(title = "Show Initial Balance High Low ", defval = true)

plot(show15highlow and  showstyle == "Default" ? orb_high : na  , style=plot.style_circles, color=orb_high[1] != orb_high ? na : color.purple, title="IB High", linewidth=2)
plot(show15highlow and  showstyle == "Default" ? orb_low  : na , style=plot.style_circles, color=orb_low[1] != orb_low ? na   : color.purple, title="IB Low",  linewidth=2)

if showstyle == "Continues Lines" and show15highlow
    label.new(DAILY_END_TIME, orb_high, "Initial Balance High - " + tostring(orb_high,"##,##,###"),xloc.bar_time, textcolor=color.purple, style=label.style_none)
    line.new(START_TIME, orb_high, DAILY_END_TIME, orb_high, xloc.bar_time, color=color.purple, width=1)
    label.new(DAILY_END_TIME, orb_low, "Initial Balance Low  -  " + tostring(orb_low,"##,##,###"),xloc.bar_time, textcolor=color.purple, style=label.style_none)
    line.new(START_TIME, orb_low, DAILY_END_TIME, orb_low, xloc.bar_time, color=color.purple, width=1) 

    

PDH = security(syminfo.tickerid, 'D', high[1], lookahead=barmerge.lookahead_on)
PDL = security(syminfo.tickerid, 'D', low[1], lookahead=barmerge.lookahead_on)
PDC = security(syminfo.tickerid, 'D', close[1], lookahead=barmerge.lookahead_on)


PDH1 = security(syminfo.tickerid, 'D', high, lookahead=barmerge.lookahead_on)
PDL1 = security(syminfo.tickerid, 'D', low, lookahead=barmerge.lookahead_on)
PDC1 = security(syminfo.tickerid, 'D', close, lookahead=barmerge.lookahead_on)


dh = security(syminfo.tickerid, 'D', high, lookahead=barmerge.lookahead_on)
dl = security(syminfo.tickerid, 'D', low, lookahead=barmerge.lookahead_on)
dc = security(syminfo.tickerid, 'D', close, lookahead=barmerge.lookahead_on)
do = security(syminfo.tickerid, 'D', open, lookahead=barmerge.lookahead_on)


tpivot = (dh + dl + dc)/3
tbc = (dh + dl)/2
ttc = (tpivot - tbc) + tpivot

pivot = (PDH + PDL + PDC)/3
bc = (PDH + PDL)/2
tc = (pivot - bc) + pivot
r1 = 2 * pivot - PDL
r2 = pivot + (PDH - PDL)
r3 = r1 + (PDH - PDL)
r4 = r3 + (r2 - r1)
s1 = 2 * pivot - PDH
s2 = pivot - (PDH - PDL)
s3 = s1 - (PDH - PDL)
s4 = s3 - (s1 - s2)


H12 = ( (PDH-PDL) * 1.90) + PDC
H11 = ( (PDH-PDL) * 1.65) + PDC
H10 = ( (PDH-PDL) * 1.55) + PDC
H9 = ( (PDH-PDL) * 1.382) + PDC
H8 = ( (PDH-PDL) * 1.125) + PDC
H7 = ( (PDH-PDL) * 1) + PDC
H6 = ( (PDH-PDL) *  .90) + PDC
H5 = ( (PDH-PDL) *  .65) + PDC
H4 = ( (PDH-PDL) *  .55) + PDC

H41 = ( (PDH1-PDL1) * .55) + PDC1

H3 = ( (PDH-PDL) * .382 ) + PDC
H2 = ( (PDH-PDL) * .30 ) + PDC
H1 = ( (PDH-PDL) * .125) + PDC

 
L1 = PDC - ( (PDH-PDL) * .125)
L2 = PDC - ( (PDH-PDL) * .30)
L3 = PDC - ( (PDH-PDL) * .382)
L4 = PDC - ( (PDH-PDL) * .55) 

L41 = PDC1 - ( (PDH1-PDL1) * .55) 

L5 = PDC - ( (PDH-PDL) * .65) 
L6 = PDC - ( (PDH-PDL) * .90)
L7 = PDC - ( (PDH-PDL) * 1)
L8 = PDC - ( (PDH-PDL) * 1.125)
L9 = PDC - ( (PDH-PDL) * 1.382)
L10 = PDC - ( (PDH-PDL) * 1.55)
L11 = PDC - ( (PDH-PDL) * 1.65)
L12 = PDC - ( (PDH-PDL) * 1.90)

showpivot = input(title = "Show Standard Pivots", defval = false)

plot(showpivot and  showstyle == "Default" ? tc     : na , title=" TC "    , style=plot.style_line, linewidth=1, color=color.fuchsia)
plot(showpivot and  showstyle == "Default" ? pivot  : na , title=" Pivot " , style=plot.style_line, linewidth=1, color=color.fuchsia)
plot(showpivot and  showstyle == "Default" ? bc     : na , title=" BC "    , style=plot.style_line, linewidth=1, color=color.fuchsia)

if showstyle == "Continues Lines" and showpivot
    label.new(DAILY_END_TIME, tc, "TC - " + tostring(tc,"##,##,###"),xloc.bar_time, textcolor=color.purple, style=label.style_none)
    line.new(START_TIME, tc, DAILY_END_TIME, tc, xloc.bar_time, color=color.purple, width=1)
    label.new(DAILY_END_TIME, pivot, "Pivot -  " + tostring(pivot,"##,##,###"),xloc.bar_time, textcolor=color.purple, style=label.style_none)
    line.new(START_TIME, pivot, DAILY_END_TIME, pivot, xloc.bar_time, color=color.purple, width=1) 
    label.new(DAILY_END_TIME, bc, "BC - " + tostring(bc,"##,##,###"),xloc.bar_time, textcolor=color.purple, style=label.style_none)
    line.new(START_TIME, bc, DAILY_END_TIME, bc, xloc.bar_time, color=color.purple, width=1)

    label.new(DAILY_END_TIME, r4, "R4 - " + tostring(r4,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, r4, DAILY_END_TIME, r4, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, r3, "R3 - " + tostring(r3,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, r3, DAILY_END_TIME, r3, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, r2, "R2 - " + tostring(r2,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, r2, DAILY_END_TIME, r2, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, r1, "R1 - " + tostring(r1,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, r1, DAILY_END_TIME, r1, xloc.bar_time, color=color.green, width=1)

    label.new(DAILY_END_TIME, s4, "S4 - " + tostring(s4,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, s4, DAILY_END_TIME, s4, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, s3, "S3 - " + tostring(r3,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, s3, DAILY_END_TIME, s3, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, s2, "S2 - " + tostring(s2,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, s2, DAILY_END_TIME, s2, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, s1, "S1 - " + tostring(s1,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, s1, DAILY_END_TIME, s1, xloc.bar_time, color=color.red, width=1)



plot(showpivot and showstyle == "Default" ? r4  : na , title=" R4 ", style=plot.style_cross, linewidth=1, color=color.green)
plot(showpivot and showstyle == "Default" ? r3  : na , title=" R3 ", style=plot.style_cross, linewidth=1, color=color.green)
plot(showpivot and showstyle == "Default" ? r2  : na , title=" R2 ", style=plot.style_cross, linewidth=1, color=color.green)
plot(showpivot and showstyle == "Default" ? r1  : na , title=" R1 ", style=plot.style_cross, linewidth=1, color=color.green)

plot(showpivot and showstyle == "Default" ? s4  : na , title=" S4 ", style=plot.style_cross, linewidth=1, color=color.red)
plot(showpivot and showstyle == "Default" ? s3  : na , title=" S3 ", style=plot.style_cross, linewidth=1, color=color.red)
plot(showpivot and showstyle == "Default" ? s2  : na , title=" S2 ", style=plot.style_cross, linewidth=1, color=color.red)
plot(showpivot and showstyle == "Default" ? s1  : na , title=" S1 ", style=plot.style_cross, linewidth=1, color=color.red)


showcampivot = input(title = "Show Camarilla Pivots", defval = true)


if showstyle == "Continues Lines" and showcampivot
    label.new(DAILY_END_TIME, H12, "Buy Bonus 4 - " + tostring(H12,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H12, DAILY_END_TIME, H12, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H11, "Buy Bonus 3 - " + tostring(H11,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H11, DAILY_END_TIME, H11, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H10, "Buy Bonus 2 - " + tostring(H10,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H10, DAILY_END_TIME, H10, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H9, "Buy Bonus 1 - " + tostring(H9,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H9, DAILY_END_TIME, H9, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H8, "Buy Target 4 - " + tostring(H8,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H8, DAILY_END_TIME, H8, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H7, "Buy Target 3 - " + tostring(H7,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H7, DAILY_END_TIME, H7, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H6, "Buy Target 2 - " + tostring(H6,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H6, DAILY_END_TIME, H6, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H5, "Buy Target 1 - " + tostring(H5,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H5, DAILY_END_TIME, H5, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H4, "Buy Above - " + tostring(H4,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H4, DAILY_END_TIME, H4, xloc.bar_time, color=color.green, width=1)
    label.new(DAILY_END_TIME, H3, "Buy Stop Loss - " + tostring(H3,"##,##,###"),xloc.bar_time, textcolor=color.green, style=label.style_none)
    line.new(START_TIME, H3, DAILY_END_TIME, H3, xloc.bar_time, color=color.green, width=1)

    label.new(DAILY_END_TIME, L12, "Sell Bonus 4 - " + tostring(L12,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L12, DAILY_END_TIME, L12, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L11, "Sell Bonus 3 - " + tostring(L11,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L11, DAILY_END_TIME, L11, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L10, "Sell Bonus 2 - " + tostring(L10,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L10, DAILY_END_TIME, L10, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L9, "Sell Bonus 1 - " + tostring(L9,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L9, DAILY_END_TIME, L9, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L8, "Sell Target 4 - " + tostring(L8,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L8, DAILY_END_TIME, L8, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L7, "Sell Target 3 - " + tostring(L7,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L7, DAILY_END_TIME, L7, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L6, "Sell Target 2 - " + tostring(L6,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L6, DAILY_END_TIME, L6, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L5, "Sell Target 1 - " + tostring(L5,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L5, DAILY_END_TIME, L5, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L4, "Sell Below - " + tostring(L4,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L4, DAILY_END_TIME, L4, xloc.bar_time, color=color.red, width=1)
    label.new(DAILY_END_TIME, L3, "Sell Stop Loss - " + tostring(L3,"##,##,###"),xloc.bar_time, textcolor=color.red, style=label.style_none)
    line.new(START_TIME, L3, DAILY_END_TIME, L3, xloc.bar_time, color=color.red, width=1)


//showcampivotinside = input(title = "Calculate Inside Camarilla Also", defval = false)


plot(showcampivot and showstyle == "Default" ? H12 : na , title=" Buy Bonus 4 ", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H11 : na , title=" Buy Bonus 3 ", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H10 : na , title=" Buy Bonus 2 ", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H9  : na , title=" Buy Bonus 1 ", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H8  : na , title=" Buy Target 4", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H7  : na , title=" Buy Target 3", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H6  : na , title=" Buy Target 2", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H5  : na , title=" Buy Target 1", style=plot.style_line, linewidth=1, color=color.green)
plot(showcampivot and showstyle == "Default" ? H4  : na , title=" Buy Above", style=plot.style_line, linewidth=2, color=color.green)
plot(showcampivot and showstyle == "Default" ? H3  : na , title=" Buy Stop Loss", style=plot.style_line, linewidth=1, color=color.green)

plot(showcampivot and showstyle == "Default" ? L12 : na , title=" Sell Bonus 4 ", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L11 : na , title=" Sell Bonus 3 ", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L10 : na , title=" Sell Bonus 2 ", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L9 : na  , title=" Sell Bonus 1 ", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L8 : na  , title=" Sell Target 4", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L7 : na  , title=" Sell Target 3", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L6 : na  , title=" Sell Target 2", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L5 : na  , title=" Sell Target 1", style=plot.style_line, linewidth=1, color=color.red)
plot(showcampivot and showstyle == "Default" ? L4 : na  , title=" Sell Below", style=plot.style_line, linewidth=2, color=color.red)
plot(showcampivot and showstyle == "Default" ? L3 : na  , title=" Sell Stop Loss", style=plot.style_line, linewidth=1, color=color.red)


showpredayvalues = input(title = "Show Previous Day High/Low/Close", defval = false)


if showstyle == "Continues Lines" and showpredayvalues
    label.new(DAILY_END_TIME, PDH, "Previous Day High - " + tostring(PDH,"##,##,###"),xloc.bar_time, textcolor=color.blue, style=label.style_none)
    line.new(START_TIME, PDH, DAILY_END_TIME, PDH, xloc.bar_time, color=color.blue, width=1)
    label.new(DAILY_END_TIME, PDL, "Previous Day Low -  " + tostring(PDL,"##,##,###"),xloc.bar_time, textcolor=color.blue, style=label.style_none)
    line.new(START_TIME, PDL, DAILY_END_TIME, PDL, xloc.bar_time, color=color.blue, width=1) 
    label.new(DAILY_END_TIME, PDC, "Previous Day Close - " + tostring(PDC,"##,##,###"),xloc.bar_time, textcolor=color.blue, style=label.style_none)
    line.new(START_TIME, PDC, DAILY_END_TIME, PDC, xloc.bar_time, color=color.blue, width=1)

    label.new(DAILY_END_TIME, PDH1, "Day High - " + tostring(PDH1,"##,##,###"),xloc.bar_time, textcolor=color.blue, style=label.style_none)
    line.new(START_TIME, PDH1, DAILY_END_TIME, PDH1, xloc.bar_time, color=color.blue, width=1)
    label.new(DAILY_END_TIME, PDL1, "Day Low - " + tostring(PDL1,"##,##,###"),xloc.bar_time, textcolor=color.blue, style=label.style_none)
    line.new(START_TIME, PDL1, DAILY_END_TIME, PDL1, xloc.bar_time, color=color.blue, width=1)


plot(showpredayvalues and showstyle == "Default" ? PDH : na, title="Prev.Day.High ", style=plot.style_line, linewidth=1, color=color.blue)
plot(showpredayvalues and showstyle == "Default" ? PDL : na, title="Prev.Day.Low  ", style=plot.style_line, linewidth=1, color=color.blue)
plot(showpredayvalues and showstyle == "Default" ? PDC : na, title="Prev.Day.Close", style=plot.style_line, linewidth=1, color=color.blue)

plot(showpredayvalues and showstyle == "Default" ? PDH1 : na, title="Day.High ", style=plot.style_circles, linewidth=1, color=color.blue)
plot(showpredayvalues and showstyle == "Default" ? PDL1 : na, title="Day.Low  ", style=plot.style_circles, linewidth=1, color=color.blue)


buy = false
sel = false

buyvwap = false
selvwap = false

volavg = false



//if showcampivotinside
//    if close < L4 and L4 < L41 
//        sel := true
//    if close > H4 and H4 > H41 
//        buy := true
//else
if close < L4  
    sel := true 
if close > H4 
    buy := true

if close < vwap
    selvwap := true
if close > vwap
    buyvwap := true

if volume > sma(volume,20)
    volavg :=  true


buysession = false
sellsession = false

timeforalert = "0915-1500"

InSession(sess) => na(time(timeframe.period, sess + ":1234567")) == false

buysession := InSession(timeforalert)
sellsession := InSession(timeforalert)



numBars = 1

t := time('D')

if t == t[1]
    numBars := nz(numBars[1]) + 1
else
    numBars := 1

for i = 1 to numBars
    if close[i] < L4
        sel := false
    if close[i] > H4
        buy := false




//plotshape( (showcampivot and sel and volt and sela ) ? ( close < L4 ) : na,style=shape.arrowdown,text="Sell",location=location.abovebar,color=color.white)
//plotshape( (showcampivot and buy and volt and buya ) ? ( close > H4 ) : na,style=shape.arrowdown,text="Buy ",location=location.belowbar,color=color.white)


plotshape( (sel and volavg and selvwap and sellsession) ? ( close < L4 ) : na,style=shape.arrowdown,text="Sell",location=location.abovebar,color=color.white)
plotshape( (buy and volavg and buyvwap and buysession ) ? ( close > H4 ) : na,style=shape.arrowdown,text="Buy ",location=location.belowbar,color=color.white)



BuyMessage = "Buy  Level Crossing for " + syminfo.ticker + "\nCurrent Closing Price is " + tostring(close,'#####.##') + "\nBuy  Above Level " + tostring(H4,'#####.##')  + "\nTarget 1 " + tostring(H5,'#####.##') + "\nTarget 2 " + tostring(H6,'#####.##') + "\nTarget 3 " + tostring(H7,'#####.##') + "\nTarget 4 " + tostring(H8,'#####.##') + "\nBonus 1 " + tostring(H9,'#####.##')  + "\nBonus 2 " + tostring(H10,'#####.##')   + "\nBonus 3 " + tostring(H11,'#####.##') + "\nBonus 4 " + tostring(H12,'#####.##') + "\n\nStop Loss " + tostring(H3,'#####.##')        
SelMessage = "Sell Level Crossing for " + syminfo.ticker + "\nCurrent Closing Price is " + tostring(close,'#####.##') + "\nSell Below Level " + tostring(L4,'#####.##')  + "\nTarget 1 " + tostring(L5,'#####.##') + "\nTarget 2 " + tostring(L6,'#####.##') + "\nTarget 3 " + tostring(L7,'#####.##') + "\nTarget 4 " + tostring(L8,'#####.##') + "\nBonus 1 " + tostring(L9,'#####.##')  + "\nBonus 2 " + tostring(L10,'#####.##')   + "\nBonus 3 " + tostring(L11,'#####.##') + "\nBonus 4 " + tostring(L12,'#####.##') + "\n\nStop Loss " + tostring(L3,'#####.##')    

 
 

if buy and volavg  and buyvwap and buysession 
    alert(BuyMessage,alert.freq_once_per_bar_close)

if sel and volavg and buyvwap and sellsession
    alert(SelMessage,alert.freq_once_per_bar_close)


