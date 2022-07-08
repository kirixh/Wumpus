domains
  % нам пригодятся следующие домены
  coord = c(integer, integer)    % координаты зала (X, Y)
  coordinates = coord*    % список координат залов
  direction = integer    % направления
  dlist = direction*    % список направлений
  strench, breeze, glitter, scream, bump = integer    % запах, ветерок, блеск, крик, удар в стену
  bumps = direction*    % список сторон зала, где есть стена
  sensors = s(strench, breeze, glitter, scream, bumps)    % набор датчиков
  wumpus, pit, gold = integer    % Вампус, яма, золото  

constants
  % направления
  right = 1    % вправо
  down = 2    % вниз
  left = 3    % влево
  up = 4      % вверх

facts - game_db  
  width(integer)  % ширина пещеры
  height(integer)  % высота пещеры
  agent(coord, direction)  % стартовая точка и начальное направление движения Агента
  wumpus(coord)  % расположение Вампуса
  pit(coord)  % координаты ямы
  gold(coord)  % координаты зала с золотом
  bump(coord, direction)  % стенки пещеры - в какую сторону из заданного зала нельзя пойти
  strench(coord)  % координаты залов, в которых чувствуется запах
  breeze(coord)  % координаты залов, в которых чувствуется ветерок
  glitter(coord)  % координаты зала, в котором виден блеск золота
  scream(integer)	% Вампус кричит - крик слышен во всех залах пещеры, поэтому координаты нам не нужны
  board_cells(coordinates)  % список координат всех залов пещеры
  board_edge_cells(coordinates)  % список координат залов, находящихся по краям пещеры
  board_corner_cells(coordinates)  % координаты залов по углам игрового поля

% База Знаний Агента
facts - agent_db
  agent_wumpus(coord)  % координаты зала, в котором находится Вампус (если они известны Агенту)
  agent_pit(coord)  % координат зала, в котором располагается яма
  agent_gold(coord)    % координаты зала с золотом
  maybe_wumpus(coord)  % предположение Агента о том, что в этом зале находится Вампус
  maybe_pit(coord)  % по мнению Агента в этом зале располагается яма
  agent_sensors(coord, sensors)  % данные датчиков в определённом зале
  safe(coord)  % зал с этими координатами безопасен
  visited(coord)  % это зал Агент уже посещал
  agent_bump(coord, direction)  % знания Агента о стенках в залах пещеры
  agent_has_gold(integer)  % у Агента на руках есть золото (1) или нет (0)
  wumpus_dead(integer)  % 1 - Вампус мёртв, 0 - где-то ещё живой
  agent_path(coordinates)	% путь, который прошел Агент в пещере
    

predicates		
  % инициализация игры
  nondeterm init_game
  % инициализация Агента
  nondeterm init_agent
  % отображение условий игры
  nondeterm show_game_db
  % установка Вампуса
  nondeterm set_wumpus
  % установка ям
  nondeterm set_pits
  % установка ямы в конкретном зале
  nondeterm set_pit(coordinates)
  % установка золота
  nondeterm set_gold
  % установка стен залов пещеры: добавим в базу данных игры информацию о том, где располагаются стенки пещеры
  nondeterm set_bumps
  % ... по левому краю пещеры
  nondeterm set_bumps_left(coordinates)
  % ... по правому краю пещеры
  nondeterm set_bumps_right(coordinates)
  % ... по нижнему краю пещеры
  nondeterm set_bumps_down(coordinates)
  % ... по верхнему краю пещеры
  nondeterm set_bumps_up(coordinates)
  % запись данных о наличии стены в зале
  nondeterm set_bump(coord, direction)
  % расстановка признаков наличия запаха
  nondeterm set_strench(coordinates)
  % расстановка признаков наличия ветерка
  nondeterm set_breeze(coordinates)
  % установка признака наличия блеска
  nondeterm set_glitter(coord)
  
  % есть запах в заданном зале?
  nondeterm is_strench(coord, strench)
  % есть ветерок в заданном зале?
  nondeterm is_breeze(coord, breeze)
  % виден ли блеск в заданном зале?																
  nondeterm is_glitter(coord, glitter)
  % слышен ли крик Вампуса?
  nondeterm is_scream(scream)
  % стены зала
  %is_bump(coord, bumps)  
  nondeterm is_bump(coord, bumps)
  
  
  
% предикаты Агента
  % анализ зала
  nondeterm analyze_cell
  % проверка не попал ли Агент в зал к Вампусу и не упал ли в яму
  nondeterm is_fail(coord)
  % анализ показаний датчиков в зале
  nondeterm analyze_sensors(coord, sensors)  
  % анализ датчика запаха
  nondeterm analyze_strench(coord, strench)
  % анализ датчика ветра
  nondeterm analyze_breeze(coord, breeze)
  % анализ датчика блеска
  nondeterm analyze_glitter(coord, glitter)
  % анализ датчика крика
  nondeterm analyze_scream(coord, scream)
  % анализ датчика удара о стену
  nondeterm analyze_bumps(coord, bumps)
  % перепроверка списка подозреваемых залов на основании Базы Знаний Агента
  nondeterm recheck_cell_list(coordinates, coordinates, string)
  % перепроверка соседних залов
  nondeterm recheck_cell(coordinates, string)
  % получение показаний датчиков
  nondeterm get_sensors(coord, sensors)
  % выборка показаний датчиков из условий задачи (если Агент впервые а этом зале)
  nondeterm get_sensors_from_game_db(coord, sensors)
  % выборка показаний датчиков из Базы Знаний Агента
  nondeterm get_sensors_from_agent_db(coord, sensors)
  % действия Агента при обнаружении в зале неприятного запаха
  nondeterm agent_set_strench(coordinates)
  % действия Агента при обнаружении в зале ветерка
  nondeterm agent_set_breeze(coordinates)
  
  nondeterm set_safe_cells(coordinates)
  nondeterm set_unsafe_cells(coordinates)
  nondeterm assume_wumpus(coordinates)
  nondeterm assume_pit(coordinates)
  nondeterm show_agent_db
  nondeterm grab_gold
  nondeterm go_back(coordinates) % возвращение назад
  nondeterm is_agent_bump(coord, direction)
  % вычисление следующего направления
  %  - по часовой стрелке (0) или против (1)
  %  - текущее направление
  %  - новое направление
  nondeterm next_direction(integer, direction, direction)
  % поворот направо
  nondeterm turnRight
  % поворот налево
  nondeterm turnLeft
  % выбираем следующий зал, куда пойдёт Агент
  nondeterm choose_next_cell
  % если безопасных залов несколько, выберем один из них
  nondeterm choose_one_cell(coordinates, coord)
  % зная координату следующего зала, проверим надо ли Агенту развернуться
  nondeterm turnIfNeed(coord, integer, coord) % параметры - координата текущего положения Агента, его направление, координата следующего зала
  % шаг вперед
  nondeterm move_forward
  % получение координат следующего зала
  nondeterm agent_next_cell(coord, direction, coord)
  
  % отображение направления в текстовом виде
  nondeterm write_direction(direction)  
  % отображение "входит/возвращается" в зависимости от того был ли уже Агент в этом зале или нет
  nondeterm write_visited(coord)
  
  nondeterm menu
  nondeterm menu_choice(char)

% вспомагательные предикаты  
  equal(integer, integer)  % сравнение целых чисел: первый параметр = второму параметру
  equal(string, string)  % сравнение строк
  equal(char, char)  % сравнение символов
  equal_list(bumps, bumps)	% сравнение списка направлений, где находится стена
  between(integer, integer, integer) % проверка находится ли первое число в промежутке между вторым и третьим числами
  nondeterm delta(char, direction, integer)

% предикаты для работы со списком залов
  nondeterm on_board(coord)  % проверяем есть ли в пещере зал с заданными координатами
  % все залы пещеры в определяемом первым параметром направлении (0 - от левого нижнего к правому верхнему, 1 - наоборот)
  %  (по условиям задачи пещера у нас прямоугольная)
  nondeterm cells_list(integer, coord, coordinates)
  % все залы пещеры
  nondeterm all_cells(coordinates)
  % следующий зал (для прохода по всем залам пещеры)
  nondeterm next_cell(integer, coord, coord)
  % залы у краёв пещеры
  nondeterm edge_cells(coordinates, coordinates)
  % угловые залы пещеры
  nondeterm corner_cells(coordinates, coordinates)  
  % соседние залы (в какие залы теоретически можно попасть из заданного зала)
  nondeterm next_cells(coord, coordinates)
  % соседние залы, которые реально существуют (с учётом ширины и высоты игрового поля)
  nondeterm only_board_cells(coordinates, coordinates)
  % соседние залы, которые по мнению Агента являются безопасными
  nondeterm next_safe_cells(coordinates, coordinates)
  
clauses
  init_game :-
    writef("Инициализация игры\n"),
    retractall(_, game_db), writef("... очищаем базу данных игры\n"),
    assertz(width(4), game_db), writef("... определили ширину пещеры\n"),
    assertz(height(4), game_db), writef("... определили высоту пещеры\n"),
    assertz(agent(c(1, 1), right), game_db), writef("... разместили Агента \n"),
    assertz(wumpus(c(1, 3)), game_db), writef("... разместили Вампуса в одном из залов пещеры\n"),
    assertz(pit(c(3, 1)), game_db),
    assertz(pit(c(3, 3)), game_db),
    assertz(pit(c(4, 4)), game_db), writef("... расставили ямы\n"),
    assertz(gold(c(2,3)), game_db), writef("... спрятали золото\n"),
    set_wumpus, writef("... установили признаки близости Вампуса\n"),
    set_pits, writef("... установили признаки близости ямы\n"),
    set_gold, writef("... установили признаки наличия золота в зале\n"),    
    assertz(scream(0), game_db), writef("... добавили факт отсутствия крика - Вампус пока жив\n"),
    all_cells(BoardCells), writef("... составили список всех залов пещеры\n"),
    assertz(board_cells(BoardCells), game_db), writef("... сохранили этот список для дальнейшего использования\n"),
    edge_cells(BoardCells, EdgeCells), writef("... выделили из общего списка залов те, которые находятся у краёв пещеры\n"),
    assertz(board_edge_cells(EdgeCells), game_db), writef("... сохранили список крайних залов для дальнейшего использования\n"),
    corner_cells(BoardCells, CornerCells), writef("... выделили из общего списка залов те, которые находятся в углах пещеры\n"),
    assertz(board_corner_cells(CornerCells), game_db), writef("... сохранили список угловых залов для дальнейшего использования\n"),
    set_bumps, writef("... определили стены пещеры\n"),
    writef("Загрузка условий игры завершена\n\n").
  
  init_agent :-
    writef("\nИнициализация Агента\n"),
    assertz(agent_has_gold(0), agent_db), writef("... у Агента нет золота\n"),
    assertz(wumpus_dead(0), agent_db), writef("... Вампус где-то в пещере жив и здоров\n"),
    agent(c(X, Y), _),
    % добавим в путь Агента первый зал
    assertz(agent_path([c(X, Y)])).
  
  show_game_db :-
    writef("\nУсловия игры:\n"),
    width(Width), height(Height),
    writef("  Ширина игрового поля %d, высота - %d\n", Width, Height),
    agent(c(Xa, Ya), Direction), writef("  Агент начинает игру в зале с координатами [%d.%d], направление - %d (", Xa, Ya, Direction), write_direction(Direction), writef(")\n"),
    wumpus(c(Xw, Yw)), writef("  Вампус находится в зале с координатами [%d.%d]\n", Xw, Yw),
    gold(c(Xg, Yg)), writef("  Золото находится в зале с координатами [%d.%d]\n", Xg, Yg),
    glitter(c(Xgl, Ygl)), writef("  Сияние можно увидеть в зале с координатами [%d.%d]\n", Xgl, Ygl),
    findall(Coord, pit(Coord), PitList), write("  Ямы расположены в следующих залах: ", PitList), nl,
    findall(Coord, breeze(Coord), BreezeList), write("  Ветерок чувствуется в этих залах: ", BreezeList), nl,
    findall(Coord, strench(Coord), StrenchList), write("  Неприятный запах ощущается в этих залах: ", StrenchList), 
    writef("\n\n******************************\n\n").
  
  % вывод на экран Базы Знаний Агента
  show_agent_db :-
    writef("\nБаза знаний Агента:\n"), 
    agent(c(X, Y), Direction),
    writef("  Сейчас Агент находится в зале %d.%d и смотрит ", X, Y), write_direction(Direction), nl,
    findall(Coord, visited(Coord), VisitedCells),
    write("  Посещённые залы: ", VisitedCells), nl,
    findall(Coord, safe(Coord), SafeCells),
    write("  Безопасные залы: ", SafeCells), nl,    
    findall(Coord, agent_wumpus(Coord), WumpusCells),
    write("  Агент знает, что тут находится Вампус: ", WumpusCells), nl,
    findall(Coord, maybe_wumpus(Coord), MaybeWumpusCells),
    write("  Агент думает, что тут может находиться Вампус: ", MaybeWumpusCells), nl,
    findall(Coord, agent_pit(Coord), PitCells),
    write("  Агент знает, что в этих залах точно располагаются ямы: ", PitCells), nl,
    findall(Coord, maybe_pit(Coord), MaybePitCells),
    write("  Агент знает, что в этих залах возможно располагаются ямы: ", MaybePitCells), nl.
%Установка препятствий и их индикаторов    
  set_wumpus :-
    wumpus(c(X, Y)),
    retractall(strench(c(X, Y)), game_db),
    assertz(strench(c(X, Y)), game_db),
    next_cells(c(X, Y), NextCells),
    only_board_cells(NextCells, NextBoardCells),
    set_strench(NextBoardCells).
    
  set_strench([]) :- !.
  set_strench([c(X, Y)|T]) :-
    retractall(strench(c(X, Y)), game_db),
    assertz(strench(c(X, Y)), game_db),
    set_strench(T).
  
  set_pits :-
    findall(Coord, pit(Coord), PitsList),
    set_pit(PitsList).
  
  set_pit([]) :- !.
  set_pit([c(X, Y)|T]) :-
    retractall(pit(c(X, Y)), game_db),
    assertz(pit(c(X, Y)), game_db),
    next_cells(c(X, Y), NextCells), only_board_cells(NextCells, NextBoardCells),
    set_breeze(NextBoardCells),
    set_pit(T).
  
  set_breeze([]) :- !.
  set_breeze([c(X, Y)|T]) :-
    retractall(breeze(c(X, Y)), game_db),
    assertz(breeze(c(X, Y)), game_db),
    set_breeze(T).
  
  set_gold :-
    gold(c(X, Y)),
    set_glitter(c(X, Y)).
  
  set_glitter(c(X, Y)) :- 
    retractall(glitter(c(X, Y)), game_db),
    assertz(glitter(c(X, Y)), game_db).
  
  set_bumps :-
    %board_edge_cells(EdgeCells),
    board_cells(EdgeCells),    
    set_bumps_left(EdgeCells),    
    set_bumps_right(EdgeCells),
    set_bumps_down(EdgeCells),
    set_bumps_up(EdgeCells).

  set_bumps_left([]).
  set_bumps_left([c(1, Y)|T]) :-    
    set_bump(c(1, Y), left),
    set_bumps_left(T), !.
  set_bumps_left([_|T]) :-
    set_bumps_left(T).
  set_bumps_right([]).
  set_bumps_right([c(Width, Y)|T]) :-
    width(Width),
    set_bump(c(Width, Y), right),
    set_bumps_right(T), !.
  set_bumps_right([_|T]) :- 
    set_bumps_right(T).
  set_bumps_down([]).
  set_bumps_down([c(X, 1)|T]) :-
    set_bump(c(X, 1), down),
    set_bumps_down(T), !.
  set_bumps_down([_|T]) :-
    set_bumps_down(T).
  set_bumps_up([]).
  set_bumps_up([c(X, Height)|T]) :-
    height(Height),
    set_bump(c(X, Height), up),
    set_bumps_up(T), !.
  set_bumps_up([_|T]) :-
    set_bumps_up(T).

  set_bump(c(X, Y), Direction) :-    
    retractall(bump(c(X, Y), Direction), game_db),
    assertz(bump(c(X, Y), Direction), game_db).
	
	write_visited(c(X, Y)) :-
		visited(c(X, Y)), write("ВОЗВРАЩАЕТСЯ").
	write_visited(_) :-
		write("ВХОДИТ").
	
	% случай, когда Агент вошел в зал, где находится Вампус, или в зал с ямой
	is_fail(c(X, Y)) :-
		wumpus(c(X, Y)),
		writef("\n\tO, ужас!\n\tАгент вошёл в зал %d.%d, а там Вампус\n\tАгент погиб\n\tИгра окончена :(\n\n", X, Y),
		% конец игры
		exit.
	is_fail(c(X, Y)) :-
		pit(c(X, Y)),
		writef("\n\tАааааааааа!\n\tАгент провалился в яму в зале %d.%d\n\tАгент погиб\n\tИгра окончена :(\n\n", X, Y),
		% конец игры
		exit.
	
	% анализируем зал, в который вошел Агент
  analyze_cell :-
    agent(c(X, Y), _Direction),
    writef("\n[analyze_cell]\tАгент "), write_visited(c(X, Y)), writef(" в зал %d.%d\n", X, Y), 
    not(is_fail(c(X, Y))),
    get_sensors(c(X, Y), s(Strench, Breeze, Glitter, Scream, Bump)),
    writef("[analyze_cell]\tпоказатели датчиков в зале %d.%d: запах - %d, ветерок - %d, блеск - %d, крик - %d, стены - ", X, Y, Strench, Breeze, Glitter, Scream), write(Bump), nl,
    retractall(visited(c(X, Y)), agent_db),
    assertz(visited(c(X, Y)), agent_db),
    writef("[analyze_cell]\tАгент анализирует показатели датчиков\n"),
  	analyze_sensors(c(X, Y), s(Strench, Breeze, Glitter, Scream, Bump)),
  	writef("[analyze_cell]\tанализа показаний датчиков завершён\n").
  
  set_safe_cells([]) :- !.
  set_safe_cells([c(X, Y)|T]) :-
    set_safe_cells(T),
    retractall(safe(c(X, Y)), agent_db),
    assertz(safe(c(X, Y)), agent_db), 
    retractall(maybe_wumpus(c(X, Y)), agent_db),
    retractall(maybe_pit(c(X, Y)), agent_db).
  
  set_unsafe_cells([]) :- !.
  set_unsafe_cells([c(X, Y)|T]) :-
    set_unsafe_cells(T),
    retractall(safe(c(X, Y)), agent_db).
  	
  % предположение - в зале находится Вампус
  assume_wumpus([]) :- !.
  assume_wumpus([c(X, Y)|T]) :-
    assume_wumpus(T),
    % проверим, а не решили ли мы ранее, что этот зал безопасен
    not(safe(c(X, Y))),    
    retractall(maybe_wumpus(c(X, Y)), agent_db),
    assertz(maybe_wumpus(c(X, Y)), agent_db),
    writef("[assume_wumpus]\tзал %d.%d помечен, как возможное расположение Вампуса\n", X, Y).
  % если ранее мы решили, что этот зал безопасен, значит в нём не может находится Вампус
  assume_wumpus([c(X, Y)|T]) :- 
    assume_wumpus(T),
    safe(c(X, Y)).
    
  % предположение - в зале располагается яма
  assume_pit([]) :- !.
  assume_pit([c(X, Y)|T]) :-
    assume_pit(T),
    % проверим, а не решили ли мы ранее, что этот зал безопасен
    not(safe(c(X, Y))),
    retractall(maybe_pit(c(X, Y)), agent_db),
    assertz(maybe_pit(c(X, Y)), agent_db),
    writef("[assume_pit]\tзал %d.%d помечен, как возможное расположение ямы\n", X, Y).
  % если ранее мы решили, что этот зал безопасен, значит в нём не может располагаться яма
  assume_pit([c(X, Y)|T]) :-
    assume_pit(T),
    safe(c(X, Y)).
  
  % берём золото
  grab_gold :-
    agent(c(X, Y), _),
    retractall(agent_gold(_), agent_db),
    assertz(agent_gold(c(X, Y)), agent_db),
    retractall(agent_has_gold(_), agent_db),
    assertz(agent_has_gold(1), agent_db),
    writef("[grab_gold]\tАгент берёт золото!\n\nСамый простой путь обратно - идти по своим следам\n"),
    agent_path(Path),
    go_back(Path).

	% возвращение к выходу из пещеры, на основании пути, пройденного Агентом
  go_back([c(X, Y)|T]) :-
		agent(c(X, Y), _),
		writef("\nПуть обратно:\n\tиз зала %d.%d Агент идёт в зал ", X, Y),
		go_back(T).
	% когда попадём в зал 1.1, можно закончить вывод пути Агента
	go_back([c(1, 1)|_]) :-
		writef("%d.%d - вот Агент и вернулся с золотом к выходу из пещеры!\n\nИгра успешно завершена\n\n", 1, 1),
		exit.
	% перечисляем залы на пути Агента
	go_back([c(X, Y)|T]) :-
		writef("%d.%d,\n\tпотом в зал ", X, Y),                                                                                                                                                     
		go_back(T).
  
  % вычисление следующего направления
  %  поворачиваем по часовой стрелке
  next_direction(0, Direction, NextDirection) :-
  	NextDirection = (Direction mod 4) + 1, !.
  %  поворачиваем против часовой стрелки
  next_direction(1, Direction, NextDirection) :-
  	Direction > 1,
  	NextDirection = Direction - 1, !.
  % для текущего направления right (1) отдельный предикат
  next_direction(1, 1, 4).
  
  % поворот... по часовой стрелке
  turnRight :-
  	writef("[turnRight]\tАгент собирается повернуть вправо\n"),
  	agent(c(X, Y), Direction),
  	next_direction(0, Direction, DirectionNew),
  	retractall(agent(c(X, Y), Direction), game_db),
  	assertz(agent(c(X, Y), DirectionNew), game_db),
  	writef("[turnRight]\tAгент в зале %d.%d развернулся направо и теперь смотрит ", X, Y), write_direction(DirectionNew), nl.
  % ... против часовой стрелки
  turnLeft :-
  	writef("[turnLeft]\tАгент собирается повернуть влево\n"),
  	agent(c(X, Y), Direction),
  	next_direction(1, Direction, DirectionNew),
  	retractall(agent(c(X, Y), Direction), game_db),
  	assertz(agent(c(X, Y), DirectionNew), game_db),
  	writef("[turnLeft]\tAгент в зале %d.%d развернулся налево и теперь смотрит ", X, Y), write_direction(DirectionNew), nl.
  	
  % выбираем следующий зал, куда пойдёт Агент
  choose_next_cell :-
  	writef("[choose_next_cell]\tАгент выбирает следующий зал\n"),  	                 														
  	agent(c(X, Y), Direction),	% смотрим, где сейчас Агент
  	agent_next_cell(c(X, Y), Direction, c(X1, Y1)), % получаем зал, куда Агент может попасть, двигаясь в том же направлении
  	% постараемся не заходить в ранее посещенные залы
  	not(visited(c(X1, Y1))),
  	writef("[choose_next_cell]\t...проверим безопасность следующего зала %d.%d\n", X1, Y1),
  	safe(c(X1, Y1)), % если следующий зал безопасен
  	writef("[choose_next_cell]\tзал впереди безопасен - Агент выбрал зал %d.%d\n", X1, Y1), !.
 	% если впереди уже посещённый зал, тогда попробуем найти другой 
  choose_next_cell :-
  	agent(c(X, Y), Direction),	% смотрим, где сейчас Агент
  	% вычисляем координаты соседних залов
  	next_cells(c(X, Y), NextCells),
  	write("[choose_next_cell]\tсписок соседних залов: ", NextCells), nl,
  	% оставляем только безопасные по мнению Агента залы
  	next_safe_cells(NextCells, NextSafeCells),
  	write("[choose_next_cell]\tиз них безопасные: ", NextSafeCells), nl,
  	% если их несколько, выбираем один
  	choose_one_cell(NextSafeCells, c(Xn, Yn)),
  	writef("[choose_next_cell]\tАгент выбрал зал %d.%d\n", Xn, Yn), 
  	turnIfNeed(c(X, Y), Direction, c(Xn, Yn)), 
  	!.
  % если следующий зале небезопасен, попадём в этот предикат
	choose_next_cell :-  	
  	writef("[choose_next_cell]\tзал впереди небезопасен. Необходимо поискать другой вариант\n"),
  	agent(c(X, Y), Direction),
		% вычисляем координаты соседних залов
  	next_cells(c(X, Y), NextCells),
  	write("[choose_next_cell]\tсписок соседних залов: ", NextCells), nl,
  	% оставляем только безопасные по мнению Агента залы
  	next_safe_cells(NextCells, NextSafeCells),
  	write("[choose_next_cell]\tиз них безопасные: ", NextSafeCells), nl,
  	% если их несколько, выбираем один
  	choose_one_cell(NextSafeCells, c(Xn, Yn)),
  	writef("[choose_next_cell]\tАгент выбрал следующий зал - %d.%d\n", Xn, Yn),
  	% проверяем необходимо ли Агенту развернуться, чтобы попасть в следующий зал
  	turnIfNeed(c(X, Y), Direction, c(Xn, Yn)).  	
  
  % если все залы уже посещались, тогда выберем последний из списка
  choose_one_cell([c(X, Y)|[]], c(X, Y)).
  % если безопасных залов несколько, выберем один из них
  choose_one_cell([c(X, Y)|_], c(X, Y)) :-
  	% предпочтём зал, в котором мы ещё не были
  	not(visited(c(X, Y))).
  % если нашли посещённый зал, посмотрим следующий зал из списка
  choose_one_cell([_|T], c(Xn, Yn)) :-
  	choose_one_cell(T, c(Xn, Yn)).  
  
  turnIfNeed(c(X, Y), _, c(X, Y)) :- !.
  turnIfNeed(c(Xa, Ya), Direction, c(Xn, Yn)) :-
  	% проверяем при текущем направлении движения попадёт ли Агент в следующий зал
  	writef("[turnIfNeed]\tпроверим, надо ли Агенту развернуться, чтобы попасть из зала %d.%d в зал %d.%d, двигаясь ", Xa, Ya, Xn, Yn), write_direction(Direction), nl,
  	agent_next_cell(c(Xa, Ya), Direction, c(Xn, Yn)), 
  	writef("[turnIfNeed]\tможно идти прямо\n"), !.
  % если не попадёт - будем разворачиваться  
  turnIfNeed(_, _, c(Xn, Yn)) :-  
  	writef("[turnIfNeed]\tнеобходимо повернуть\n"),
  	turnRight,
  	agent(c(Xa1, Ya1), Direction1),
  	turnIfNeed(c(Xa1, Ya1), Direction1, c(Xn, Yn)).  	
  
	% вычисляем коэффициент изменения координат по направлению движения
	%  для координаты X
  delta('X', 1, 1). % вправо - координата X увеличивается
  delta('X', 3, -1). % влево - координата X уменьшается  
  delta('X', _, 0). % для направления вверх и вниз - дельта = 0
 	%  для координаты Y
  delta('Y', 2, -1) :- !.
  delta('Y', 4, 1) :- !.
  delta('Y', _, 0).
  	
  % получение координат следующего зала  
  agent_next_cell(c(X, Y), Direction, c(Xn, Yn)) :-
  	not(is_agent_bump(c(X, Y), Direction)),
  	delta('X', Direction, Dx), 
  	delta('Y', Direction, Dy),
  	Xn = X + Dx, 
  	Yn = Y + Dy, !.
  % если Агент упрётся в стену, сообщим об этом
	agent_next_cell(c(X, Y), Direction, c(Xn, Yn)) :-
		agent(c(X, Y), Direction),
		is_agent_bump(c(X, Y), Direction),
		writef("[agent_next_cell]\t...Агент не может идти из зала %d.%d ", X, Y), write_direction(Direction), writef(" - впереди стена\n"),
		retractall(agent_bump(c(X, Y), Direction), agent_db),
		assertz(agent_bump(c(X, Y), Direction), agent_db), 
		turnRight,
		agent(c(Xa, Ya), DirectionA),
		agent_next_cell(c(Xa, Ya), DirectionA, c(Xn, Yn)).
	% иначе - просто сделаем ещё один поворот
	agent_next_cell(_, _, c(Xn, Yn)) :-
		turnRight,
		agent(c(Xa, Ya), DirectionA),
		agent_next_cell(c(Xa, Ya), DirectionA, c(Xn, Yn)).
  
  % движение вперед
  move_forward :-
  	writef("[move_forward]\tАгент продвигается вперед\n"),
  	agent(c(X, Y), Direction),
  	agent_next_cell(c(X, Y), Direction, c(Xn, Yn)),  	
  	retractall(agent(c(X, Y), Direction), game_db),
  	assertz(agent(c(Xn, Yn), Direction), game_db), 
  	
  	agent_path(Path),
  	NewPath = [c(Xn, Yn)|Path],
  	retractall(agent_path(_), agent_db),
  	
  	assertz(agent_path(NewPath), agent_db),
  	writef("[move_forward]\tАгент перешел из зала %d.%d в зал %d.%d\n", X, Y, Xn, Yn), !.
  
  % перед тем, как заподозрить зал в том, что в нём есть яма или Вампус
  %  попробуем проверить соседние посещённые ячейки на наличие в них соответствующих признаков - ветерка и запаха
  recheck_cell_list([], [], _).
  recheck_cell_list([c(X, Y)|T1], [c(X, Y)|T], Param2Check) :-
  	next_cells(c(X, Y), NextCells2Check),
  	not(recheck_cell(NextCells2Check, Param2Check)),
  	recheck_cell_list(T1, T, Param2Check).
  recheck_cell_list([c(X, Y)|T1], T, Param2Check) :-
  	writef("[recheck_cell_list]\tзал %d.%d не прошел перепроверку, поэтому пометим его как безопасный\n", X, Y),
  	set_safe_cells([c(X, Y)]),
  	recheck_cell_list(T1, T, Param2Check).
  
  % если среди соседних есть хоть один посещенный зал, в котором не чувствовался запах, тогда такой зал надо признать безопасным  
  recheck_cell([c(X, Y)|_], "Strench") :-
  	visited(c(X, Y)), not(is_strench(c(X, Y), 1)), 
  	writef("[recheck_cell]\tАгент уже был с зале %d.%d и не почувствовал там запаха - здесь не может находиться Вампус\n", X, Y).
  recheck_cell([c(X, Y)|_], "Breeze") :-
  	visited(c(X, Y)), not(is_breeze(c(X, Y), 1)), 
  	writef("[recheck_cell]\tАгент уже был с зале %d.%d и не почувствовал там ветерка - здесь не может быть ямы\n", X, Y).
  recheck_cell([_|T], Param2Check) :-  	
  	recheck_cell(T, Param2Check).
  	
  
  % есть сияние
  analyze_sensors(c(X, Y), s(_, _, 1, _, _)) :-
    writef("\n[analyze_sensors, Glitter]\tв зале %d.%d Агент увидел сияние\n\tУра!!!\n\tАгент нашёл золото!!!\n\tМожно брать его и отправляться в обратный путь\n\n", X, Y),
    grab_gold.
  % нет ни запаха, ни ветерка
  analyze_sensors(c(X, Y), s(0, 0, _, _, _)) :-
    next_cells(c(X, Y), NextCells), 
    writef("[analyze_sensors]\tв зале %d.%d не чувствуется ни запах, ни ветерок - значит соседние залы ", X, Y), write(NextCells), writef(" безопасны\n"),
    % ячейка, в которой находится Агент, безопасна
    set_safe_cells([c(X, Y)]),
    % соседние ячейки тоже безопасны
    set_safe_cells(NextCells).  
  % есть запах
  analyze_sensors(c(X, Y), s(1, _Breeze, _Glitter, _Scream, _Bumps)) :-
    next_cells(c(X, Y), NextCells), 
    next_safe_cells(NextCells, NextSafeCells),
    writef("[analyze_sensors, Strench]\tв зале %d.%d чувствуется запах - значит где-то в одном из соседних залов ", X, Y), write(NextCells), writef(" находится Вампус!\n"),
    writef("[analyze_sensors, Strench]\tно на основании предыдущих исследований Агент знает, что эти залы безопасны: "), write(NextSafeCells), nl,
    % перепроверим предположение, обратившись к Базе Знаний Агента
    recheck_cell_list(NextCells, NextCells2, "Strench"),
    % сделаем предположение, что в этих залах находится Вампус
    writef("[analyze_sensors]\tпометим следующие залы, как возможное расположение Вампуса: "), write(NextCells2), nl,
    assume_wumpus(NextCells2).
  % есть ветерок
  analyze_sensors(c(X, Y), s(_Strench, 1, _Glitter, _Scream, _Bumps)) :-
    next_cells(c(X, Y), NextCells), 
    next_safe_cells(NextCells, NextSafeCells),
    writef("[analyze_sensors, Breeze]\tв зале %d.%d я чувствую ветерок - значит где-то в одном из соседних залов ", X, Y), write(NextCells), writef(" располагается яма!\n"),
    writef("[analyze_sensors, Breeze]\tно на основании предыдущих исследований Агент знает, что эти залы безопасны: "), write(NextSafeCells), nl,
    % перепроверим предположение, обратившись к Базе Знаний Агента
    recheck_cell_list(NextCells, NextCells2, "Breeze"),
    % сделаем предположение, что в этих залах находится яма
    writef("[analyze_sensors]\tпометим следующие залы, как возможное расположение ямы: "), write(NextCells2), nl,
    assume_pit(NextCells2).  
  % слышен крик Вампуса
  analyze_sensors(c(X, Y), s(_, _, _, 1, _)) :-
    writef("[analyze_sensors, Scream]\tнахожусь в зале %d.%d и слышу как кричит Вампус, значит он мёртв. Ура! Ходить по пещере стало безопаснее\n", X, Y),
		% сохраним информацию об этом событии
		retractall(wumpus_dead(_), agent_db),
		assertz(wumpus_dead(1), agent_db),
		% можно удалить информацию о крике - она нужна только один раз
		retractall(scream(_), game_db).
	analyze_sensors(c(X, Y), s(_, _, _, _, Bumps)) :-
		not(equal_list(Bumps, [])),
		agent(c(X, Y), Direction),
		writef("[analyze_cells, Bump]\tАгент: попытался пойти в зале %d.%d ", X, Y), write_direction(Direction), writef(" и наткнулся на стену - надо повернуть\n"),
		turnRight.

  analyze_sensors(c(X, Y), s(Strench, Breeze, Glitter, Scream, Bumps)) :-  	
    analyze_strench(c(X, Y), Strench),
    analyze_breeze(c(X, Y), Breeze),
    analyze_glitter(c(X, Y), Glitter),
    analyze_scream(c(X, Y), Scream),
    analyze_bumps(c(X, Y), Bumps), !.
  
  analyze_strench(c(X, Y), 1) :-
    next_cells(c(X, Y), NextCells),
    agent_set_strench(NextCells).
  analyze_strench(_, 0).
  
  analyze_breeze(c(X, Y), 1) :-
  	next_cells(c(X, Y), NextCells),
  	agent_set_breeze(NextCells).
  analyze_breeze(_, 0).
  
  analyze_glitter(c(X, Y), 1) :-
  	writef("[analyze_glitter]\tУра! Агент нашел золото в зале %d.%d\nМожно возвращаться назад\n", X, Y).
  analyze_glitter(_, 0).

  analyze_scream(c(X, Y), 1) :-
  	writef("[analyze_scream]\tАгент находится в зале %d.%d и слышит как кричит Вампус - Вампус мёртв!\n", X, Y).  
  analyze_scream(_, 0).

	analyze_bumps(_, []).
  analyze_bumps(c(X, Y), [Bump|T]) :- 
  	writef("[analyze_bumps]\tВ зале %d.%d можно наткнуться на стену, двигаясь ", X, Y), write_direction(Bump), nl,
  	analyze_bumps(c(X, Y), T).
  
  agent_set_strench([]) :- !.
  agent_set_strench([c(X, Y)|T]) :-
    % если не уверены, что соседний зал безопасен
    not(safe(c(X, Y))),
    % тогда сделаем предположение, что там находится Вампус
		retractall(maybe_wumpus(c(X, Y)), agent_db),
		assertz(maybe_wumpus(c(X, Y)), agent_db),
		agent_set_strench(T).
	
	agent_set_breeze([]) :- !.
	agent_set_breeze([c(X, Y)|T]) :-
		% если не уверены, что соседний зал безопасен
    not(safe(c(X, Y))),
    % тогда сделаем предположение, что там располагается яма
		retractall(maybe_pit(c(X, Y)), agent_db),
		assertz(maybe_pit(c(X, Y)), agent_db),
		agent_set_breeze(T).
  
  get_sensors(c(X, Y), Sensors) :-
    not(visited(c(X, Y))),
    get_sensors_from_game_db(c(X, Y), Sensors), !.
    
  get_sensors(c(X, Y), Sensors) :- 
  	get_sensors_from_agent_db(c(X, Y), Sensors).
  
  is_strench(c(X, Y), 1) :- strench(c(X, Y)), !.
  is_strench(_, 0).

  is_breeze(c(X, Y), 1) :- breeze(c(X, Y)), !.
  is_breeze(_, 0).
  
  is_glitter(c(X, Y), 1) :- glitter(c(X, Y)), !.
  is_glitter(_, 0).
  
  is_scream(Scream) :- scream(Scream).
  
  is_bump(c(X, Y), []) :- 
  	findall(Direction, bump(c(X, Y), Direction), Bumps), 
    equal_list(Bumps, []), !.
  is_bump(c(X, Y), Bumps) :-
    findall(Direction, bump(c(X, Y), Direction), Bumps).     
  is_agent_bump(c(X, Y), Direction) :-
  	bump(c(X, Y), Direction).
  is_agent_bump(c(X, Y), Direction) :-
  	agent_bump(c(X, Y), Direction).
  
  get_sensors_from_game_db(c(X, Y), s(Strench, Breeze, Glitter, Scream, Bump)) :-
    is_strench(c(X, Y), Strench),
    is_breeze(c(X, Y), Breeze),
    is_glitter(c(X, Y), Glitter),
    is_bump(c(X, Y), Bump),
    is_scream(Scream).
  
  get_sensors_from_agent_db(c(X, Y), s(Strench, Breeze, Glitter, Scream, Bump)) :-
    % возьмём данные из базы данных игры
    get_sensors_from_game_db(c(X, Y), s(Strench, Breeze, Glitter, Scream, Bump)).
    
	% отображение направления движения в текстовом виде
	write_direction(1) :- write("вправо"), !.
	write_direction(2) :- write("вниз"), !.
	write_direction(3) :- write("влево"), !.
	write_direction(4) :- write("вверх"), !.
	

  % проверка равенства (для целых чисел и строк)
  equal(X, X).
  % ... для списков
  equal_list([], []).
  equal_list([X|T1], [X|T2]) :- equal_list(T1, T2).

  % проверка находится ли число 
  between(A, B, C) :- A >= B, A <=C.

% предикаты для работы со списком залов
  % проверка есть ли зал с заданными координатами в пещере
  on_board(c(X, Y)) :-    
    width(Width), height(Height),
    between(X, 1, Width),
    between(Y, 1, Height).
  
  % соседние залы (теоретически возможные)
  next_cells(c(X, Y), NextCells) :-
    X1 = X - 1, X2 = X + 1, Y1 = Y - 1, Y2 = Y + 1,
    NC1 = [c(X1, Y)], NC2 = [c(X2, Y)|NC1], NC3 = [c(X, Y1)|NC2],
    NextCells = [c(X, Y2)|NC3].
  
  % проверяем все ли залы есть в пещере
  only_board_cells([], []) :- !.
  only_board_cells([c(X, Y)|T1], [c(X, Y)|T2]) :-
    on_board(c(X, Y)),
    only_board_cells(T1, T2), !.
  % если зала нет в пещере, попадаем в этот предикат
  only_board_cells([_|T1], T2) :-
    only_board_cells(T1, T2).  
  
  % выбираем безопасные, по мнению Агента, залы из списка
  next_safe_cells([], []) :- !.
  next_safe_cells([c(X, Y)|T1], [c(X, Y)|T2]) :-
  	safe(c(X, Y)), not(maybe_wumpus(c(X, Y))), not(maybe_pit(c(X, Y))),
  	next_safe_cells(T1, T2), !.
  % если зал не безопасен, попадаем в этот предикат
  next_safe_cells([_|T1], T2) :-
  	next_safe_cells(T1, T2).
  
    
  next_cell(0, c(X, Y), c(Xnext, Y)) :-
    width(Width), Xnext = X + 1, Xnext <= Width.
  next_cell(0, c(X, Y), c(Xnext, Ynext)) :-
    width(Width), height(Height), Xtemp = X + 1, Xtemp > Width, Xnext = 1, Ynext = Y + 1,  Ynext <= Height.
  next_cell(0, c(Width, Height), c(0, 0)) :-
    width(Width), height(Height).
  next_cell(1, c(X, Y), c(Xnext, Y)) :-
    width(Width), Xnext = X + 1, Xnext <= Width.
  next_cell(1, c(X, Y), c(Xnext, Ynext)) :-
    width(Width), Xtemp = X + 1, Xtemp > Width, Xnext = 1, Ynext = Y - 1,  Ynext >= 1.
  next_cell(1, c(Width, 1), c(0, 0)) :-
    width(Width).
    
  cells_list(0, c(X, Y), [c(X, Y)]) :- 
    next_cell(0, c(X, Y), c(0, 0)), !.
  cells_list(0, c(X, Y), [c(X, Y)|T]) :-
    next_cell(0, c(X, Y), c(Xn, Yn)),
    cells_list(0, c(Xn, Yn), T).
  cells_list(1, c(X, Y), [c(X, Y)]) :- 
    next_cell(1, c(X, Y), c(0, 0)), !.
  cells_list(1, c(X, Y), [c(X, Y)|T]) :-
    next_cell(1, c(X, Y), c(Xn, Yn)),
    cells_list(1, c(Xn, Yn), T).  
    
  all_cells(CellsListDown) :- 
    height(Height),
    cells_list(1, c(1, Height), CellsListDown).
    
  edge_cells([c(X, Y)|T1], [c(X, Y)|T]) :-
    height(Height), equal(X, 1), not(equal(Y, 1)), not(equal(Y, Height)), edge_cells(T1, T);
    width(Width), height(Height), equal(X, Width), not(equal(Y, 1)), not(equal(Y, Height)), edge_cells(T1, T);
    width(Width), equal(Y, 1), not(equal(X, 1)), not(equal(X, Width)), edge_cells(T1, T);
    width(Width), height(Height), equal(Y, Height), not(equal(X, 1)), not(equal(X, Width)), edge_cells(T1, T), !.
  edge_cells([_|T1], T) :-
    edge_cells(T1, T).
  edge_cells([], []).
  
  corner_cells([c(X, Y)|T1], [c(X, Y)|T]) :-
    equal(X, 1), equal(Y, 1), corner_cells(T1, T);
    width(Width), height(Height), equal(X, Width), equal(Y, Height), corner_cells(T1, T);
    width(Width), equal(Y, 1), equal(X, Width), corner_cells(T1, T);
    height(Height), equal(Y, Height), equal(X, 1), corner_cells(T1, T), !.
  corner_cells([_|T1], T) :-
    corner_cells(T1, T).
  corner_cells([], []).
  
  menu :-
  	writef("\n___________\n Меню игры:\n"),
  	writef("\t1 - следующий шаг\n\t2 - показать Базу Знаний Агента\n\n\t0 - выход из игры\n"),
  	writef("ваш выбор: "), readchar(Choice), write(Choice), writef("\n~~~~~~~~~~~\n\n"), menu_choice(Choice).
    
  menu_choice('1') :- choose_next_cell, move_forward, analyze_cell, menu.
 	menu_choice('2') :- show_agent_db, menu.
 	menu_choice('0') :- writef("\n\nИгра окончена\nДо новых встреч\n"), exit.
 	menu_choice(_) :- writef("Выберите, пожалуйста, один из предложенных вариантов\n"), menu.	
  
goal
  init_game,
  init_agent,
  show_game_db,
  writef("\nНачинаем игру\n"),
  analyze_cell,
  menu,
  !, fail.