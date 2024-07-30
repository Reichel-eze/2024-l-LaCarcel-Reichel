% La Carcel

% De los guardias sabemos su nombre y de los prisioneros, su nombre y los crímenes que cometieron. 
% Se contemplan tres tipos de crímenes:
% - Homicidio, que registra la víctima.
% - Narcotráfico, que indica la lista de drogas traficadas.
% - Robo, que relaciona cuánto dinero en dólares fue robado.

% guardia(Nombre)
guardia(bennett).
guardia(mendez).
guardia(george).

% prisionero(Nombre, Crimen)
prisionero(piper, narcotrafico([metanfetaminas])).
prisionero(alex, narcotrafico([heroina])).
prisionero(alex, homicidio(george)).
prisionero(red, homicidio(rusoMafioso)).
prisionero(suzanne, robo(450000)).
prisionero(suzanne, robo(250000)).
prisionero(suzanne, robo(2500)).
prisionero(dayanara, narcotrafico([heroina, opio])).
prisionero(dayanara, narcotrafico([metanfetaminas])).

% 1) Dado el predicado controla/2:

% controla(Controlador, Controlado)
controla(piper, alex).          % es un hecho (inversible)
controla(bennett, dayanara).    % es un hecho (inversible)
controla(Guardia, Otro):- 
    prisionero(Otro,_), 
    guardia(Guardia),           % PARA QUE SEA INVERSIBLE
    not(controla(Otro, Guardia)).

% Indicar, justificando, si es inversible y, en caso de no serlo, 
% dar ejemplos de las consultas que NO podrían hacerse y corregir 
% la implementación para que se pueda.

% El predicado controla(Guardia, Otro) NO es inversible porque la
% varible/incognita Guardia se utiliza por primera vez dentro de un
% not (el not NO es capaz de ligarlo) (NO esta unificada/ligada), 
% mientras tanto para la variable Otro si existe la inversibilidad

% El predicado controla/2 es inversible para el 2do parametro Otro 
% pero NO para el primero!!

% Para convertir el predicado en complementamente inversible, tenemos
% que ligar/unificar la variable Guardia antes que sea utilizada en el
% not

% consultas que no podian hacerse:
% ?- controla(Guardia,piper). --> false.
% ?- controla(Guardia,red). ---> ERROR

% 2) conflictoDeIntereses/2: relaciona a dos personas distintas 
% (ya sean guardias o prisioneros) si no se controlan mutuamente 
% y existe algún tercero al cual ambos controlan
conflictoDeIntereses(Uno, Otro) :-
    %esPersona(Uno),   % para que sea inversible
    %esPersona(Otro),  % para que sea inversible  
    controla(Uno,Tercero),  % asi es mas facil lo de inversible (ligo la variable) (primero esto, despues el not)
    controla(Otro,Tercero), % asi es mas facil lo de inversible (ligo la variable)
    not(controla(Uno,Otro)),
    not(controla(Otro,Uno)),
    Uno \= Otro.

% 3) peligroso/1: Se cumple para un preso que sólo cometió crímenes graves.
% Un robo nunca es grave.
% Un homicidio siempre es grave.
% Un delito de narcotráfico es grave cuando incluye al menos 5 drogas a la vez, o incluye metanfetaminas.
peligroso(Preso) :-
    prisionero(Preso,_), % TENGO QUE LIGAR!! (ANTES DE LLEGAR AL FORALL)
    forall(prisionero(Preso,Crimen), crimenGrave(Crimen)). % Para todos los crimenes cometidos por el prisionero, dichos crimenes son graves

% HOLA POLIMORFISMO!!
crimenGrave(homicidio(_)).

crimenGrave(narcotrafico(ListaDeDrogas)) :-
    length(ListaDeDrogas, Cantidad),
    Cantidad >= 5.

crimenGrave(narcotrafico(ListaDeDrogas)) :-
    member(metanfetaminas, ListaDeDrogas).

% 4) ladronDeGuanteBlanco/1: Aplica a un prisionero si sólo cometió 
% robos y todos fueron por más de $100.000.

ladronDeGuanteBlanco(Prisionero) :-
    prisionero(Prisionero, _),
    forall(prisionero(Prisionero, Crimen), (monto(Crimen, Monto), Monto > 100000)).
                                            % tiene que tener un monto(es decir, ser un robo el crimen) y que ese monto sea mayor a 100000
monto(robo(Monto), Monto).

% 5) condena/2: Relaciona a un prisionero con la cantidad de años de condena que debe cumplir. 
% Esto se calcula como la suma de los años que le aporte cada crimen cometido, que se obtienen de la siguiente forma:
% - La cantidad de dinero robado dividido 10.000.
% - 7 años por cada homicidio cometido, más 2 años extra si la víctima era un guardia.
% - 2 años por cada droga que haya traficado.

condena(Prisionero, CantidadCondena) :-
    prisionero(Prisionero,_), % acordarse de ligarlo para que sea inversible
    findall(AniosCrimen, (prisionero(Prisionero,Crimen), penaSegunCrimen(Crimen, AniosCrimen)), ListaDeAnios),
    sum_list(ListaDeAnios, CantidadCondena).
    
penaSegunCrimen(robo(Monto), AniosCrimen) :- AniosCrimen is Monto / 10000.

penaSegunCrimen(homicidio(Persona), 7) :- not(guardia(Persona)).
    %AniosCrimen is 7. (esta al pedo esto, lo puedo poner directamente arriba)

penaSegunCrimen(homicidio(Persona), 9) :- guardia(Persona).
    %AniosCrimen is 9. (esta al pedo esto, lo puedo poner directamente arriba)

penaSegunCrimen(narcotrafico(ListaDeDrogas), AniosCrimen) :-
    length(ListaDeDrogas, Cantidad),
    AniosCrimen is Cantidad * 2.
    
% 6) capoDiTutiLiCapi/1: Se dice que un preso es el capo de todos los capos cuando nadie lo controla, pero todas las 
% personas de la cárcel (guardias o prisioneros) son controlados por él, o por alguien a quien él controla (directa o indirectamente).

capoDiTutiLiCapi(Preso) :-
    prisionero(Preso,_),        % para la inversibilidad
    not(controla(_,Preso)),     % NO existe nadie que lo controle
    forall((esPersona(Alguien), Alguien \= Preso), controlacion(Preso,Alguien)).

esPersona(Persona) :- guardia(Persona).
esPersona(Persona) :- prisionero(Persona,_).

controlacion(Controlador, Controlado) :-
    controla(Controlador, Controlado).      % el caso base (control directo)

controlacion(Controlador, Controlado) :-
    controla(Controlador, Intermedio),      % el caso recursivo (control indirecto)
    controlacion(Intermedio, Controlado).