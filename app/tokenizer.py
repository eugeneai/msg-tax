import ucto
from json import load, loads, dumps
import pymorphy2
import inspect
import getopt
import sys
import logging

logging.basicConfig(filename='tokenizer.log',
                    encoding='utf-8',
                    level=logging.DEBUG)

if not hasattr(inspect, 'getargspec'):
    inspect.getargspec = inspect.getfullargspec

TOPICS = """seo — SEO
buhtg — Бухгалтерия, источник — ТГ
motion — motion-дизайн. Короткие ролики для соц.сетей и т.д.
buhvk — Бухгалтерия, источник — ВК
chat-bots — Чат-боты
copywrite — Копирайт, работа с текстами
tilda — Создание сайтов на конструкторе Tilda
avito — Авито
agoryachev — Индивидуальная тематика под человека. Поиск персонала в общепит
konstantin_dt — Индивидуальная тематика
design — Графический дизайн
smm — SMM и все связанное с ним
directvk — Контекстная реклама, источник — ВК
direct — Контекстная реклама, источник — ТГ
video — Видеомонтаж
realtyrf — Недвижимость России
juristtg — Юриспруденция, источник — ТГ
graphic design — Графический дизайн
realtymsk — Недвижимость Москвы
sites — Сайты
carelles — Индивидуальная тематика
dara — Индивидуальная тематика
dasofron — Индивидуальная тематика
ff-target — Тергетированная реклама. Тематика для другого бота (FF)
produce — Продюсирование
ff-sites — Сайты. Тематика для другого бота (FF)
Adv_Place — Индивидуальная тематика
ff-direct — Контекстная реклама. Тематика для другого бота (FF)
ff-storymakers — Сторисмейкеры (15-секундные истории для соц.сетей). Тематика для другого бота (FF)
target — Таргетированная реклама
putov — Индивидуальная тематика
ff-smm — SMM. Тематика для другого бота (FF)
ff-copywrite — Копирайт. Тематика для другого бота (FF)
mix — Digital. Сборная тематика, состоящая из ключей по сайтам, таргету. контексту, копирайту, дизайну и т.д."""

DT = {}


def prepDT():
    global DT
    lines = TOPICS.split("\n")
    for line in lines:
        try:
            k, v = line.split("—", 1)
            DT[k.strip()] = v.strip()
        except ValueError:
            print("Bad {}".format(line))


def gen(filename):
    i = open(filename)
    js = load(i)
    for e in js:
        yield e


morph = pymorphy2.MorphAnalyzer(lang='ru')


def morphy(tok):
    s = str(tok)
    t = tok.tokentype
    p = morph.parse(s)
    l = []
    for f in p:
        # print(f)
        ml = str(f.tag).split(",")
        mln = []
        for ttt in ml:
            if " " in ttt:
                mln.append([_.lower() for _ in ttt.split()])
            else:
                mln.append(ttt.lower())
        d = {"norm": f.normal_form, "tag": mln, "score": f.score}
        l.append(d)
    # print(l)
    return l


configfile = "tokconfig-rus"


def tokenize(t):
    tokenizer = ucto.Tokenizer(configfile)
    tokenizer.process(t)
    toks = []
    for tok in tokenizer:
        # toks.append(tok)
        s = str(tok)
        tt = tok.tokentype
        o = {"w": s, "ucto": tt.lower()}
        if tt in ["WORD", "WORD-COMPOUND"]:
            o["morph"] = morphy(tok)
        toks.append(o)

    return toks


def mainlearn():
    fromfile("data/posts.json")


DEBUG = False


def fromfile(filename):
    for js in gen(filename):
        try:
            t = js["text"]
            s = js["subjects"]
            li = []
            for k, v in s.items():
                dc = {}
                dc["name"] = k
                dc["tax"] = v["classes"]
                li.append(dc)
            nt = tokenize(t)
            njs = {"text": nt, "subjects": li}
            print(dumps(njs, ensure_ascii=False))
        except KeyError:
            print("BAD")
            print(dumps(js, ensure_ascii=False))
        if DEBUG:
            break


def piping():
    while True:
        cmd = input()
        # print("~"+cmd)
        logging.info("CMD: " + cmd)
        if cmd.startswith("WORD"):
            _, w = cmd.split("WORD", 1)
            w = w.strip()
            tkn = tokenize(w)
            js = dumps(tkn, ensure_ascii=False)
            print("{0:010d} ".format(len(js)), end="")
            print(js)
            sys.stdout.flush()
            logging.info("RES:" + js + "\n" + str(tkn))
        if cmd == "QUIT":
            return


if __name__ == "__main__":
    prepDT()
    # print(DT.keys())
    args = sys.argv[1:]
    optlist, args = getopt.getopt(args, 'i:w:s:pdvt')
    for o, v in optlist:
        if o == "-d":
            DEBUG = True
        elif o == "-i":
            fromfile(v)
        elif o == "-w":
            word(v)
        elif o == "-s":
            sent(v)
        elif o == "-v":
            print("""Version string\n\n""")
        elif o == "-p":
            piping()
        elif o == "-t":
            mainlearn()
    # print(optlist, args)
