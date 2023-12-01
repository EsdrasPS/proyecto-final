Algoritmo proyecto_final
	//Variables
	Definir estudiantes, op, carrera, maestro, salon, long Como Entero
	//Contadores de los profesores asignados a sus salones
	Definir contProfeMario, contProfeAviles, contMaestraSheyla, contMaestraErendira, contProfeLumbreras, contMaestraKaren, contProfeMascorro Como Entero
	Definir nombre_estudiante, grupo, edificio, guion, mostrar_datos, becas, pagos Como Caracter
	Definir encontrado, mayus, datos_ingresados Como Logico
	Dimension carreras[8], maestros[7] //Vector donde se guardan las carreras de la universidad y sus maestros que la imparten
	encontrado <- Falso
	mayus <- Falso
	datos_ingresados <- Falso
	//Carreras que ofrece la universidad
	carreras[0] <- "ITI"
	carreras[1] <- "IM"
	carreras[2] <- "ISA"
	carreras[3] <- "LAGE"
	carreras[4] <- "LCIA"
	carreras[5] <- "ITM"
	//Maestros que pertenecen a la Universidad
	maestros[0] <- "Mario Humberto Rodriguez Chavez"
	maestros[1] <- "Hector Aviles"
	maestros[2] <- "Arturo Guadalupe Mascorro Cienfuegos"
	maestros[3] <- "Erendira Gutierrez Meza"
	maestros[4] <- "Juan Diego Lumbreras Vega"
	maestros[5] <- "Sheyla Maleny Silva Vega"
	maestros[6] <- "Karen Priscila"
	//Menú Principal
	Escribir "<---SISTEMA DE GESTOR ESCOLAR--->"
	Escribir "1.- Ingresar Alumno"
	Escribir "2.- Mostrar Datos de los Alumnos"
	Escribir Sin Saltar "Opcion: "; Leer op //Ingreso de la opción
	Si op <= 0 o op > 2 //Validación de las opciones
		Repetir //Repetición de la peticion de la carrera en caso de estar fuera del rango
			Escribir "Opcion no disponible"
			Escribir Sin Saltar "Opcion: "; Leer op
		Hasta Que op == 1 o op == 2
	SiNo //Opciones del menú
		Segun op
			Caso 1: //En caso de presionar opcion 1
				Escribir Sin Saltar "Cantidad de estudiantes: "; Leer estudiantes //Ingreso de la cantidad de alumnos a registrar
				Dimension universidad[estudiantes,7] //Creación de la matriz donde se guarda la información de los alumnos
				Para i<-0 Hasta estudiantes-1 Con Paso 1 Hacer //Ciclo donde se ingresa la información de los alumnos
					Escribir "<---ESTUDIANTE ",i+1,"--->" //Contador de los alumnos
					Para j<-0 Hasta 0 Con Paso 1 Hacer
						Escribir Sin Saltar "Nombre Completo: "; Leer estudiante //Nombre del estudiante
						Si universidad[i,0] == estudiante //Validacion en caso de que se repita el nombre del alumno
							Repetir
								Escribir "Estudiante ya existente" //Repeticion del ingreso del nombre del alumno
								Escribir Sin Saltar "Nombre: "; Leer estudiante
							Hasta Que estudiante <> universidad[i,j]
						SiNo
							universidad[i,0] <- estudiante //En caso de que sea distinto el nombre del alumno, automaticamente se guarda en la matriz
						FinSi
						Escribir Sin Saltar "Carrera: "; Leer carrera //Ingreso de la carrera donde pertenece el alumno
						Si (carrera <= 0 o carrera > 6) //Validación en caso de que el número de la carrera sea negativo, cero y mayores a 6
							Repetir
								Escribir "Carrera no reconocida" //Repeticion del ingreso de la carrera del alumno
								Escribir Sin Saltar "Carrera: "; Leer carrera
							Hasta Que carrera >= 1 y carrera <= 6
						SiNo
							Segun carrera Hacer //Dependiendo del número ingresado (1-6) se escoje la carrera
								Caso 1: universidad[i,1] <- carreras[0] //Si presiono 1, el alumno pertenece a ITI
								Caso 2: universidad[i,1] <- carreras[1] //Si presiono 2, el alumno pertenece a Mecatronica
								Caso 3: universidad[i,1] <- carreras[2] //Si presiono 3, el alumno pertenece a Sistemas Automotrices
								Caso 4: universidad[i,1] <- carreras[3] //Si presiono 4, el alumno pertenece a LAIG
								Caso 5: universidad[i,1] <- carreras[4] //Si presiono 5, el alumno pertenece a LCIA
								Caso 6: universidad[i,1] <- carreras[5] //Si presiono 6, el alumno pertenece a Manufacturas
							FinSegun
						FinSi
						Escribir Sin Saltar "Grupo: "; Leer grupo //Ingreso del grupo donde toma clase el alumno
						long <- Longitud(grupo) //Cuenta la longitud de la variable del grupo
						guion <- Subcadena(grupo,1,1) //Detecta el guion que separa la letra del edificio con el número del salón
						Si (guion == "-" y long == 5) //En caso de que lo ingresado ya sea mayuscula y se detecte el guion no ocurre nada
							mayus = Verdadero
						FinSi
						Si (mayus = Falso)
							Repetir //Validacion en caso de que lo ingresado no sea mayuscula y no encuentre el guion
								Escribir "Te equivocaste en el ingreso del grupo"
								Escribir Sin Saltar "Grupo: "; Leer grupo //Ingreso del grupo donde toma clase el alumno
								long <- Longitud(grupo)
								guion <- Subcadena(grupo,1,1)
							Hasta Que long == 5 y guion == "-"
						FinSi
						universidad[i,2] <- Mayusculas(grupo)
						Escribir Sin Saltar "Maestros: "; Leer maestro
						Si (maestro <= 0 o maestro > 7)
							Repetir
								Escribir "Maestro no reconocido"
								Escribir Sin Saltar "Maestro: "; Leer maestro
							Hasta Que maestro >= 1 y maestro <= 7
						SiNo
							Segun maestro 
								Caso 1: 
									universidad[i,3] <- maestros[0] //Si presiona 1, el maestro es Mario Humberto Rodriguez Chavez
									contProfeMario <- contProfeMario + 1
								Caso 2: 
									universidad[i,3] <- maestros[1] //Si presiona 2, el maestro es Hector Aviles
									contProfeAviles <- contProfeAviles + 1
								Caso 3: 
									universidad[i,3] <- maestros[2] //Si presiona 3, el maestro es Arturo Guadalupe Mascorro Cienfuegos
									contProfeMascorro <- contProfeMascorro + 1
								Caso 4: 
									universidad[i,3] <- maestros[3] //Si presiona 4, el maestro es Erendira Gutierrez Meza
									contMaestraErendira <- contMaestraErendira + 1
								Caso 5: 
									universidad[i,3] <- maestros[4] //Si presiona 5, el maestro es Juan Diego Lumbreras Vega
									contProfeLumbreras <- contProfeLumbreras + 1
								Caso 6: 
									universidad[i,3] <- maestros[5] //Si presiona 6, el maestro es Sheyla Maleny Silva Vega
									contMaestraSheyla <- contMaestraSheyla +  1
								Caso 7: 
									universidad[i,3] <- maestros[6] //Si presiona 7, el maestro es Karen Priscila
									contMaestraKaren <- contMaestraKaren + 1
							FinSegun
						FinSi
						Escribir Sin Saltar "Pagos: "; Leer pagos //Ingreso de que si el alumno cumplio con el pago del cuatrimestre
						Si (pagos == "") //Validacion en caso de que el campo este vacio
							Repetir
								Escribir "Debes ingresar algo (si/no)"
								Escribir Sin Saltar "Pagos: "; Leer pagos
							Hasta Que pagos == "si" o pagos == "no"
						FinSi
						Si (pagos == "si") //En caso de una respuesta afirmativa
							universidad[i,4] <- "Pago cumplido"
						SiNo
							Si (pagos == "no") //En caso de una respuesta negativa
								Escribir "Corres el gran riesgo de no quedar inscrito"
								universidad[i,4] <- "No cumplio con el pago"
							FinSi
						FinSi
						Escribir Sin Saltar "Becas: "; Leer becas //Ingreso de que si el alumno cuenta con una beca
						Si (becas == "") //En caso de que el campo este vacio
							Repetir
								Escribir "Debes de proporcionar algo o respuesta invalida"
								Escribir Sin Saltar "Becas: "; Leer becas
							Hasta Que becas == "si" o becas == "no"
						SiNo
							Si (becas <> "si" y becas <> "no") //En caso de que se haya proporcionado respuestas distintas a si y no
								Repetir
									Escribir "Debes de proporcionar algo o respuesta invalida"
									Escribir Sin Saltar "Becas: "; Leer becas
								Hasta Que becas == "si" o becas == "no"
							FinSi
						FinSi
						Si ((becas == "si" y pagos == "no") o (becas=="si" y pagos=="si"))//Si el alumno cuenta con una beca y no pago el cuatri, automaticamente se usa la beca para pagar el cuatri o sea cumplio con el pago gracias a la beca
							universidad[i,4] <- "Cumplio con el pago"
							universidad[i,5] <- "Alumno con beca"
							universidad[i,6] <- "Inscripcion satisfactoria"
						FinSi
						Si (becas == "no" y pagos == "si") //Si el alumno cumple con el pago de cuatri pero no cuenta con beca, no es necesario que necesite de una beca ya que puede cumplir con los pagos
							universidad[i,4] <- "Cumplio con el pago"
							universidad[i,5] <- "No cuenta con beca"
							universidad[i,6] <- "Inscripcion satisfactoria"
						FinSi
					FinPara
				FinPara
				datos_ingresados <- Verdadero //Significa que ahora si hay datos en la matriz de la universidad
				Escribir "Deseas mostrar lo ingresado de los alumnos inscritos? (si/no): "; Leer mostrar_datos //Pregunta al usuario si quiere mostrar lo ingresado a la matriz
				Si (mostrar_datos == "si")
					Si datos_ingresados == Falso //Validacion en caso de que al inicio del menu presionemos 2 
						Escribir "No tienes nada ingresado en la matriz"
					SiNo
						Para i<-0 Hasta estudiantes-1 Con Paso 1 Hacer
							Escribir "<---DATOS DEL ESTUDIANTE ",i+1,"--->"
							Para j<-0 Hasta 0 Con Paso 1 Hacer
								Si (universidad[i,j] <> "")
									Escribir "Nombre del estudiante: ",universidad[i,0]
									Escribir "Carrera escogida: ", universidad[i,1]
									Escribir "Grupo asignado: ", universidad[i,2]
									Escribir "Maestro asignado: ", universidad[i,3]
									Escribir "Estatus de Pago: ", universidad[i,4]
									Escribir "Estatus de Beca: ", universidad[i,5]
									Escribir "Estatus de Inscripcion: ", universidad[i,6]
								FinSi
							FinPara
						FinPara
					FinSi
				FinSi
				Si (mostrar_datos == "no")
					Escribir "Dijiste que no"
				FinSi
			Caso 2:
				Si datos_ingresados == Falso //Validacion en caso de que al inicio del menu presionemos 2 
					Escribir "No tienes nada ingresado en la matriz"
				SiNo
					Para i<-0 Hasta estudiantes-1 Con Paso 1 Hacer
						Escribir "<---DATOS DEL ESTUDIANTE ",i+1,"--->"
						Para j<-0 Hasta 0 Con Paso 1 Hacer
							Si (universidad[i,j] <> "")
								Escribir "Nombre del estudiante: ",universidad[i,0]
								Escribir "Carrera escogida: ", universidad[i,1]
								Escribir "Grupo asignado: ", universidad[i,2]
								Escribir "Maestro asignado: ", universidad[i,3]
								Escribir "Estatus de Pago: ", universidad[i,4]
								Escribir "Estatus de Beca: ", universidad[i,5]
								Escribir "Estatus de Inscripcion: ", universidad[i,6]
							FinSi
						FinPara
					FinPara
				FinSi
		FinSegun
	FinSi
FinAlgoritmo