.PHONY: run build check prep repl sent

PYTARGET=app/tokenizer.py
PKG=msg-tax

run: build check
	python $(PYTARGET) -d -i data/posts.json | cabal v2-run msg-tax -- -t

build:
	cabal build

check: $(PYTARGET)
	python -m py_compile $^

prep: check
	python $(PYTARGET)

repl: build
	cabal v2-repl exe:$(PKG)


sent: build
	# cabal v2-run msg-tax -- -s "Стали"
	# cabal v2-run msg-tax -- -s "Мама мыла раму. Отец стирал шкаф. Дети играли."
	#cabal v2-run msg-tax -- -s "Мама мыла раму"
	#cabal v2-run msg-tax -- -s "штекольно"
	#cabal v2-run msg-tax -- -s "Глокая куздра штекольно поднула куздрёнка."
	cabal v2-run msg-tax -- -s "Глокая куздра поднула штекольно куздрёнка."
	# cabal v2-run msg-tax -- -s "Высокооплачиваемая работа для девушек в Тюмень, Ханты-Мансийск, Сургут и города ХМАО, ЯНАО. Прямой работодатель. Работы много. Мы ищем не сотрудницу, а партнёра, которому можно доверять."
#" Отдадим предпочтение: 1) Aмбициозной профессионалке, готовой получать удовольствие от работы. 2) Двум подружкам без вредных привычек, для работы в паре - вдвоём на апартаментах. 3) Девушкам с регионов, готовых работать с проживанием вахтовым методом от 10 дней вахта! За 10 дней Вы сможете заработать 200 000 руб!!! У Нас очень простые и понятные условия работы: Мы работаем с Вами 50/50. Если у тебя есть доп. услуги- это твои деньги. Расчёт каждый час!!! Фотогроаф и фотосеесии для нас не проблема!!! График работы подстраивается под Вас. -Никаких штрафов и вычетов - ИСКЛЮЧЕНО. -Обширная база постоянный клиентов - только граждане РФ. -Безопасность обеспечена на все 100%. -Апартаменты предоставляются."
