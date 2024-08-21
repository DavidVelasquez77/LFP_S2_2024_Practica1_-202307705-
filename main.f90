module productoModule
    type :: equipo 
    character(len=256) :: nombre
    integer:: cantidad
    real:: precio_unitario
    character(len=256):: ubicacion
    contains
        procedure :: inicializar
        !procedure :: mostrar 
        procedure :: agregarStock
        procedure :: quitarStock

    end type equipo
    contains
    !Inicializar
    subroutine inicializar(this, nombre, cantidad, precio_unitario, ubicacion)
        class (equipo), intent(inout) :: this
        character(len=256), intent (in) :: nombre
        integer, intent (in) :: cantidad
        real, intent (in) :: precio_unitario
        character(len=256), intent (in) :: ubicacion
        this%nombre = nombre
        this%cantidad = cantidad
        this%precio_unitario = precio_unitario
        this%ubicacion = ubicacion  

        end subroutine inicializar 

!agregar stock
        subroutine agregarStock(this, cantidad)
            class(equipo), intent(inout) :: this
            integer, intent(in) :: cantidad
            this%cantidad = this%cantidad + cantidad
            end subroutine agregarStock
!quitar stock
            subroutine quitarStock(this, cantidad)
                class(equipo), intent(inout) :: this
                integer, intent(in) :: cantidad
                this%cantidad = this%cantidad-cantidad
                end subroutine quitarStock 


end module productoModule

module global_vars
use productoModule
integer :: n=1
integer :: i
type (equipo), dimension (150) :: inventario  
end module global_vars

program main
    use productoModule
    use global_vars
    implicit  none 
    integer:: op !variable para leer la opcion


    do
        PRINT *, '****************************************'
        PRINT *, '         SISTEMA DE INVENTARIO          '
        PRINT *, '****************************************'
        PRINT *, '1. Cargar Inventario Inicial'
        PRINT *, '2. Cargar Instrucciones de Movimientos'
        PRINT *, '3. Crear Informe de Inventario'
        PRINT *, '4. Salir'
        PRINT *, '****************************************'
        PRINT *, 'Seleccione una opcion (1-4):'
        PRINT *, '****************************************'
        READ (*, *) op
        select case (op)
        case (1)
            call CrearArchivo()
        case (2)
            call AccionesArchivo()
        case (3)
            call CrearInformeInventario()        
        case (4)
            print *, "Saliendo del sistema..."
            do i=1, n-1
                print *, "Equipo: ", i
                print *, "Nombre: ", inventario(i)%nombre
                print *, "cantidad: ", inventario(i)%cantidad
                print *, "Precio: ", inventario(i)%precio_unitario
                print *, "Ubicacion: ", inventario(i)%ubicacion
            end do
            stop
        case (5)
            call system ("cls")
        case default 
            print *, "Opcion no valida"
        end select 
    end do

end program main

subroutine CrearArchivo()
    use productoModule
    use global_vars

    integer :: iunit, ios, pos, cantidad_int
    real :: precio_real
    character(len=256) :: nombre, cantidad, precio_unitario, ubicacion, linea, comando
    character(len=256) :: archivoEntrada

    print *, "****************************************"
    print *, "*   Ingrese la ruta del archivo de     *"
    print *, "*         inventario inicial:          *"
    print *, "****************************************"
    read *, archivoEntrada  

    ! Asignando unidades
    iunit = 10
    open(unit=iunit, file=trim(archivoEntrada), status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print *, "Error al abrir el archivo: ", archivoEntrada
        stop    
    end if

    do 
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)
        
        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, ' ')
        if (pos > 0) then
            comando = linea(1:pos-1)
            linea = trim(linea(pos+1:))
            ! Separar por ;
            pos = index(linea, ';')
            if (pos > 0) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))
                ! Siguiente atributo
                pos = index(linea, ';')
                if (pos > 0) then
                    cantidad = linea(1:pos-1)
                    linea = trim(linea(pos+1:))
                    read(cantidad, '(I10)', iostat=ios) cantidad_int
                    ! Siguiente atributo
                    pos = index(linea, ';')
                    if (pos > 0) then
                        precio_unitario = linea(1:pos-1)
                        read(precio_unitario, '(F10.2)', iostat=ios) precio_real
                        ubicacion = trim(linea(pos+1:))

                        if (comando == "crear_equipo") then
                            call crearProducto(nombre, cantidad_int, precio_real, ubicacion)
                            ! Mostrar información del equipo creado en el formato solicitado
                            print *, "****************************************"
                            print *, "Nombre: ", trim(nombre)
                            print *, "Cantidad: ", cantidad_int
                            print *, "Precio: ", precio_real
                            print *, "Ubicacion: ", trim(ubicacion)
                            print *, "****************************************"
                        end if

                    end if
                end if
            end if
        end if
    end do
    close(unit=iunit)
end subroutine CrearArchivo



subroutine crearProducto(nombre, cantidad, precio_unitario, ubicacion)
    use productoModule
    use global_vars
    !dummy
    character(len=256), intent (in) :: nombre
    integer, intent (in) :: cantidad
    real, intent (in) :: precio_unitario
    character(len=256), intent (in) :: ubicacion
    type(equipo) :: nuevoEquipo
    call nuevoEquipo%inicializar(nombre, cantidad, precio_unitario, ubicacion)
    inventario(n) = nuevoEquipo
    n= n + 1
 
    read *
end subroutine crearProducto

subroutine AccionesArchivo()
    use productoModule
    use global_vars

    integer :: iunit, ios, pos, cantidad_int
    character(len=256) :: nombre, ubicacion, linea, comando
    character(len=256) :: archivoAcciones

    print *, "****************************************"
    print *, "*   Ingrese la ruta del archivo de     *"
    print *, "*    instrucciones de movimientos:     *"
    print *, "****************************************"
    read *, archivoAcciones

    ! Archivo en modo lectura
    iunit = 11
    open(unit=iunit, file=trim(archivoAcciones), status="old", action="read", iostat=ios)

    ! Verificar si hay error al abrir el archivo
    if (ios /= 0) then
        print *, "****************************************"
        print *, "*  Error al abrir el archivo:          *"
        print *, "* ", trim(archivoAcciones), " *"
        print *, "****************************************"
        stop
    end if

    print *, "****************************************"
    print *, "*  Procesando archivo de acciones:     *"
    print *, "* ", trim(archivoAcciones), " *"
    print *, "****************************************"

    do
        read(iunit, '(A)', iostat=ios) linea
        if (ios /= 0) exit
        linea = trim(linea)
        
        ! Imprimir línea leída con formato
        print *, "----------------------------------------"
        print *, " Leyendo linea: ", trim(adjustl(linea))
        print *, "----------------------------------------"

        ! Encuentra el primer espacio para extraer el comando
        pos = index(linea, ' ')
        if (pos > 0) then
            comando = trim(linea(1:pos-1))
            linea = trim(linea(pos+1:))
            ! Separar por ;
            pos = index(linea, ';')
            if (pos > 0) then
                nombre = trim(linea(1:pos-1))
                linea = trim(linea(pos+1:))
                ! Siguiente atributo
                pos = index(linea, ';')
                if (pos > 0) then
                    read(linea(1:pos-1), '(I10)', iostat=ios) cantidad_int
                    ubicacion = trim(linea(pos+1:))

                    if (comando == "agregar_stock") then
                        call agregar_stock(nombre, cantidad_int, ubicacion)
                    else if (comando == "eliminar_equipo") then
                        call eliminar_equipo(nombre, cantidad_int, ubicacion)
                    end if
                end if
            end if
        end if
    end do

    print *, "****************************************"
    print *, "*       Procesamiento completado       *"
    print *, "****************************************"

    close(unit=iunit)
end subroutine AccionesArchivo




subroutine agregar_stock(nombre, cantidad, ubicacion)
    use productoModule
    use global_vars
    !dummy
    character(len=256), intent (in) :: nombre
    integer, intent (in) :: cantidad
    character(len=256), intent (in) :: ubicacion
    logical :: encontrado = .false.

    do i=1, n-1
        if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(inventario(i)%ubicacion) == trim(ubicacion)) then
            call inventario(i)%agregarStock(cantidad)
            encontrado = .true.
            print *, "Stock agregado a ", trim(nombre), " en ", trim(ubicacion)
            print *, "Nueva cantidad: ", inventario(i)%cantidad
        end if
    end do

    if (.not. encontrado) then
        print *, "No se encontró el producto ", trim(nombre), " en la ubicación ", trim(ubicacion)
    end if
end subroutine agregar_stock

subroutine eliminar_equipo(nombre, cantidad, ubicacion)
    use productoModule
    use global_vars
    character(len=256), intent(in) :: nombre
    integer, intent(in) :: cantidad
    character(len=256), intent(in) :: ubicacion
    logical :: encontrado = .false.

    do i = 1, n - 1
        if (trim(inventario(i)%nombre) == trim(nombre) .and. trim(inventario(i)%ubicacion) == trim(ubicacion)) then
            encontrado = .true.
            if (cantidad <= inventario(i)%cantidad) then
                call inventario(i)%quitarStock(cantidad)
                print *, "Stock reducido de ", trim(nombre), " en ", trim(ubicacion)
                print *, "Nueva cantidad: ", inventario(i)%cantidad
            else
                print *, "Error: La cantidad a eliminar es mayor que la cantidad existente en ", trim(ubicacion)
            end if
        end if
    end do

    if (.not. encontrado) then
        print *, "No se encontró el producto ", trim(nombre), " en la ubicación ", trim(ubicacion)
    end if
end subroutine eliminar_equipo

subroutine CrearInformeInventario()
    use productoModule
    use global_vars
    implicit none
    integer :: iunit, j
    real :: valor_total
    character(len=256) :: archivoSalida

    archivoSalida = 'informe.txt'
    iunit = 20

    ! Abrir el archivo para escribir
    open(unit=iunit, file=trim(archivoSalida), status='replace', action='write')

    ! Escribir encabezado del informe con formato
    write(iunit, '(A)') '********************************************************************************'
    write(iunit, '(A)') '*                        Informe de Inventario Actual                          *'
    write(iunit, '(A)') '********************************************************************************'
    write(iunit, '(A)') '* Equipo               | Cantidad | Precio Unit | Valor Total | Ubicación      *'
    write (iunit,'(A)') '--------------------------------------------------------------------------------'

    ! Escribir los detalles del inventario
    do j = 1, n-1
        valor_total = inventario(j)%cantidad * inventario(j)%precio_unitario
        write(iunit, '(A, T22, I7, T32, F11.2, T45, F12.2, T60, A)') &
            trim(adjustl(inventario(j)%nombre)), inventario(j)%cantidad, inventario(j)%precio_unitario, valor_total, trim(adjustl(inventario(j)%ubicacion))
    end do

    ! Cerrar el archivo
    close(unit=iunit)

    ! Mensaje de confirmación
    print *, "Informe de inventario creado en", trim(archivoSalida)
end subroutine CrearInformeInventario
