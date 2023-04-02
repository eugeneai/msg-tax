import ucto
from json import load, loads, dumps

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


if __name__ == "__main__":
    prepDT()
    # print(DT.keys())
    for js in gen("../data/posts.json"):
        try:
            t = js["text"]
            s = js["subjects"]
            li = []
            for k, v in s.items():
                dc = {}
                dc["name"] = k
                # dc.update(v)
                dc["tax"] = v["classes"]
                li.append(dc)

            njs = {"text": t, "subjects": li}
            print(dumps(njs, ensure_ascii=False))
        except KeyError:
            print("BAD")
            print(dumps(js, ensure_ascii=False))
