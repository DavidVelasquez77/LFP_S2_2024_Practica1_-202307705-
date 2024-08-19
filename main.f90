module InventarioModule
    implicit none 

    type :: Inventario
        character(len=50) :: nombre
        character(len=50) :: ubicacion
        integer :: cantidad
        real :: precio_unitario 
    end type Inventario

    integer, parameter :: MAX_INVENTARIO = 100

end module InventarioModule

    program gestionar_inventario
        use InventarioModule
        type (Inventario), dimension(:), allocatable :: inventarios(:)
        character(len=100) :: filename
        integer :: num_inventario
        integer :: opcion

        do
            PRINT *, '****************************************'
            PRINT *, '         SISTEMA DE INVENTARIO          '
            PRINT *, '****************************************'
            PRINT *, '1. Cargar Inventario'
            PRINT *, '2. Cargar Movimientos'
            PRINT *, '3. Mostrar Inventario'
            PRINT *, '4. Salir'
            PRINT *, '****************************************'
            PRINT *, 'Seleccione una opcion (1-4):'
            PRINT *, '****************************************'
            READ (*, *) opcion   
            
            select case (opcion)
                case (1)
                    PRINT *, 'Ingrese el nombre de la ruta del archivo de inventario:'
                    READ *,  filename
                    call analizador(filename,inventarios)
                case (2)
                    PRINT *, 'Ingrese el nombre de la ruta del archivo de movimientos:'
                    READ *, filename
                case (3)
                    PRINT *, 'Ingrese el nombre de la ruta:'
                    READ *, filename
                case (4)
                    PRINT *, 'Saliendo del sistema'
                    exit
                case default
                    PRINT *, 'Opcion no valida'
            end select
        end do

        contains
        SUBROUTINE parse_line(line, inventario1)
            use InventarioModule, only: Inventario
            type(Inventario), intent(out):: inventario1
            CHARACTER(LEN=*), INTENT(IN) :: line
            INTEGER :: i 
            CHARACTER(LEN=200) :: temp_line 
            INTEGER :: start, end_pos 
            CHARACTER(LEN=50) :: field(4)
    
            temp_line = line
            start = 1
    
            !separar las lineas en campos por ;
            DO i = 1,4
                end_pos = INDEX(temp_line(start:), ';')
                IF (end_pos == 0 .AND. i == 4) THEN
                    field(i) = temp_line(start:)
                ELSE    
                    field(i) = temp_line(start:start+end_pos-2)
                    start = start + end_pos
                END IF
            END DO
            
            inventario1%nombre = TRIM(field(1))
            inventario1%ubicacion = TRIM(field(2))
            READ(field(3), '(I10)') inventario1%cantidad
            READ(field(4), '(I10)') inventario1%precio_unitario
        
        
            
        END SUBROUTINE parse_line

        SUBROUTINE parse_line3(line)
            CHARACTER(LEN=*), INTENT(IN) :: line
            INTEGER :: i 
            CHARACTER(LEN=200) :: temp_line 
            INTEGER :: start, end_pos 
            CHARACTER(LEN=50) :: field(3)
    
            temp_line = line
            start = 1
    
            !separar las lineas en campos por ;
            DO i = 1,3
                end_pos = INDEX(temp_line(start:), ';')
                IF (end_pos == 0 .AND. i == 3) THEN
                    field(i) = temp_line(start:)
                ELSE    
                    field(i) = temp_line(start:start+end_pos-2)
                    start = start + end_pos
                END IF
            END DO
            
            print *, field(1)
            print *, field(2)
            print *, field(3)
            
        END SUBROUTINE parse_line3


            SUBROUTINE analizador(archivo, inventarios)

                character(len=100) :: archivo ! nombre del archivo
                character(len=100) :: line !variable para leer linea por linea
                integer :: ios !error del archivo
                character(len=50) :: comando !comando a ejecutar agregar stocker, eleminar stock
                character(len=50) :: datos !obtener la linea
                integer :: i, start, end_pos ! variables para leer y separ datos
                integer :: contador
                integer :: num_inventario


                open(unit=10, file=archivo, status='old', action='read', iostat=ios)
                if (ios /= 0) then
                    PRINT *, 'Error al abrir el archivo'
                    stop
                end if

                contador = 0

                do
                    read(10, '(A)', iostat=ios) line
                    if (ios /= 0) EXIT
                    start = 1
                    end_pos = SCAN(line(start:), ' ')
                    IF (end_pos == 0) THEN
                        comando = TRIM(line(start:))
                        datos = ''
                    ELSE 
                        comando = TRIM(line(start:start+end_pos-2))
                        datos = TRIM(line(start+end_pos:))
                    END IF

                    select case (comando)
                    case ('crear_equipo')
                        print *, 'Crear equipo'
                        call parse_line(datos, inventarios(contador))
                        contador = contador + 1

                    case ('agregar_stock')
                        print *, 'Agregar stock'
                        call parse_line3(datos)
                    case ('eliminar_stock')
                        print *, 'Eliminar stock'
                        print *, datos
                    case default
                        print *, 'Comando no valido'
                    end select

                end do
            end SUBROUTINE analizador

    end program gestionar_inventario
