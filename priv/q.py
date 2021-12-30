#!/usr/bin/env python3

import os,sys,json,time,re
from datetime import datetime,date
from http import client as hc
from colorama import Fore,Style


HOST='127.0.0.1'
PORT=7000
TIME_RE = r"((?:\d\d\.)?(?:\d\d)\.(?:\d\d))?(?:([+-])?(\d+)[dD][aA][yY])?"
SIG_INVITE_RE = r"^(INVITE)\s*(\S+)(\s*SIP/2\.0)$"
SIG_HEADER_RE = r"^([^:]+)\:(.+)$"


DAY_MILLISECONDS = 86_400_000


def usage():
    print('''
    q.py   <method>  <query_id>  <time_range>
    method:  (session | s) | (caller | c) | (callee | e)
    query_id:   session_id | caller_num | callee_num
    time_range:   yy.mm.dd[Nday] |  mm.dd[Nday]

    Example:
        q.py session some_session  04.12
        q.py caller 123456789   04.12-8day
        q.py callee 987654321   04.12+8day
    ''')

def timestamp(time_range):
    def parse_date(dstr):
        if dstr is None:
            t = date.today()
            return int(datetime(t.year,t.month,t.day).timestamp() * 1000)
        if (len(dstr) == 5):     # year is not included
            year = date.today().year
            (month,day) = dstr.split('.')
        else:
            (year,month,day) = dstr.split('.')
        dargs = [int(x) for x in (year,month,day)]
        return int(datetime(*dargs).timestamp() * 1000)

    m = re.match(TIME_RE,time_range)
    if m is None:
        raise Exception('invalid time range')
    else:
        start = parse_date(m.group(1))
        sign = m.group(2)
        days = m.group(3)
        if days is None:
            days = 1
        if sign is None:
            tsend = start + DAY_MILLISECONDS # within 1 day
        elif sign == '+':
            tsend = start + int(days) * DAY_MILLISECONDS
        elif sign == '-':
            tsend = start - int(days) * DAY_MILLISECONDS
        else:
            raise Exception('wrong sign in time_range')
        return sorted((start,tsend))

def url(method,query_id,time_range):
    ts = timestamp(time_range)
    return 'http://%s:%d/%s?id=%s&ts_start=%s&ts_end=%s' % (HOST,PORT,method,query_id,ts[0],ts[1])

def format_session(session):
    sid = session['session_id']
    caller = session['caller']
    callee = session['callee']
    timestamp = session['timestamp']
    date = datetime.fromtimestamp(timestamp // 1000)
    date_str = date.strftime('[%m/%d] %H:%M:%S')
    return Fore.BLUE + '#' * 80 + '\n' + \
        '|%s| caller(%s) --> callee(%s)  created: %s' % (sid,caller,callee,date_str) + \
        '\n' + '#' * 80 + Fore.RESET

def format_signal_list(sig_list):
    formatted = ''
    for sig in sig_list:
        data_str = datetime.fromtimestamp(sig['timestamp'] // 1000).strftime('%H:%M:%S')
        sig_title = Fore.YELLOW + '<%s>    %s' % (sig['seq'],data_str) + Fore.RESET
        output = [sig_title]
        for s in sig['packet'].split('\r\n'):
            color1 = Style.BRIGHT
            color2 = Style.NORMAL + Fore.WHITE
            m = re.match(SIG_INVITE_RE,s)
            if m is not None:
                color1 = Style.BRIGHT + Fore.RED
                color2 = Style.BRIGHT + Fore.YELLOW
                output.append(color1 + m.group(1) + ' ' + color2 + m.group(2) + \
                    Style.RESET_ALL + Fore.RESET + m.group(3))
            else:
                m = re.match(SIG_HEADER_RE,s)
                if m is None:
                    output.append(s)
                else:
                    header = m.group(1).lower()
                    if header == 'from' or header == 'to':
                        color1 += Fore.GREEN
                    elif '=' in header: # sdp properties
                        color1 = Style.NORMAL
                    output.append(color1 + m.group(1) + ':' + color2 + m.group(2))
        formatted += '\r\n'.join(output)
    return formatted

def query(method,query_id,time_range):
    req_url = url(method,query_id,time_range)
    #print('request url is ',req_url)
    conn = hc.HTTPConnection(HOST,PORT)
    conn.request('GET',req_url)
    resp = conn.getresponse()
    if resp.status == 200:
        data = json.loads(resp.read())
        pretty_print(data)

def pretty_print(session_list):
    for l in session_list:
        session = l['session']
        signal_list = l['signal']
        print(format_session(session))
        print(format_signal_list(signal_list))

def main():
    try:
        if (len(sys.argv) == 3):
            sys.argv.append(' ')
        (method,query_id,time_range) = sys.argv[1:4]
        if method == 'session' or method == 's':
            query('get_session',query_id,time_range)
        elif method == 'caller' or method == 'c':
            query('get_caller',query_id,time_range)
        elif method == 'callee' or method == 'e':
            query('get_callee',query_id,time_range)
        else:
            raise Exception('invalid method')
    except ValueError:
        usage()
    except Exception as e:
        print(e.with_traceback())
        usage()


if __name__ == '__main__':
    main()
