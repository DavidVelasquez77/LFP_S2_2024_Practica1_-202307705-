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
    type(Inventario), dimension(:), allocatable :: inventarios
    character(len=100) :: filename
    integer :: num_inventario = 0
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
                READ *, filename
                call analizador(filename)
            case (2)
                PRINT *, 'Ingrese el nombre de la ruta del archivo de movimientos:'
                READ *, filename
                call analizador(filename)  ! Aquí llamamos al analizador también para los movimientos
            case (3)
                call mostrar_inventario()
            case (4)
                PRINT *, 'Saliendo del sistema'
                exit
            case default
                PRINT *, 'Opcion no valida'
        end select
    end do

contains

    SUBROUTINE parse_line(line)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: i 
        CHARACTER(LEN=200) :: temp_line 
        INTEGER :: start, end_pos 
        CHARACTER(LEN=50) :: field(4)

        temp_line = line
        start = 1

        ! Separar las líneas en campos por ;
        DO i = 1, 4
            end_pos = INDEX(temp_line(start:), ';')
            IF (end_pos == 0 .AND. i == 4) THEN
                field(i) = temp_line(start:)
            ELSE    
                field(i) = temp_line(start:start+end_pos-2)
                start = start + end_pos
            END IF
        END DO
        
        print *, 'Datos extraidos:'
        print *, 'Nombre: ', field(1)
        print *, 'Cantidad: ', field(2)
        print *, 'Precio Unitario: ', field(3)
        print *, 'Ubicacion: ', field(4)
        print *, '---------------------------------------'
    END SUBROUTINE parse_line

    SUBROUTINE parse_line3(line)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: i 
        CHARACTER(LEN=200) :: temp_line 
        INTEGER :: start, end_pos 
        CHARACTER(LEN=50) :: field(3)

        temp_line = line
        start = 1

        ! Separar las líneas en campos por ;
        DO i = 1, 3
            end_pos = INDEX(temp_line(start:), ';')
            IF (end_pos == 0 .AND. i == 3) THEN
                field(i) = temp_line(start:)
            ELSE    
                field(i) = temp_line(start:start+end_pos-2)
                start = start + end_pos
            END IF
        END DO
        
        print *, 'Datos extraidos para movimientos:'
        print *, 'Nombre: ', field(1)
        print *, 'Cantidad: ', field(2)
        print *, 'Ubicacion: ', field(3)
        print *, '---------------------------------------'
    END SUBROUTINE parse_line3

    SUBROUTINE analizador(archivo)
        character(len=100) :: archivo
        character(len=100) :: line
        integer :: ios
        character(len=50) :: comando
        character(len=50) :: datos
        integer :: start, end_pos

        open(unit=10, file=archivo, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            PRINT *, 'Error al abrir el archivo'
            stop
        end if

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
                print *, '---------------------------------------'
                PRINT *, 'Comando valido: Crear equipo'
                call parse_line(datos)
            case ('agregar_stock')
                        print *, '---------------------------------------'
                PRINT *, 'Comando valido: Agregar stock'
                call parse_line3(datos)
            case ('eliminar_equipo')
                        print *, '---------------------------------------'
                PRINT *, 'Comando valido: Eliminar stock'
                call parse_line3(datos)
            case default
                PRINT *, 'Comando no valido'
            end select

        end do
        close(10)
    end SUBROUTINE analizador

    SUBROUTINE mostrar_inventario()
        ! Implementación para mostrar el inventario
        PRINT *, 'Función mostrar inventario aún no implementada.'
    END SUBROUTINE mostrar_inventario

end program gestionar_inventario
