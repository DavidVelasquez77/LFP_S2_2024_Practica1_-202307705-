module equipoModule
    type :: equipo 
        character(len=256) :: descripcion
        integer :: stock
        real :: precio
        character(len=256) :: almacen
        contains
            procedure :: inicializarEquipo
            procedure :: aumentarStock
            procedure :: disminuirStock
    end type equipo

contains

    ! Subrutina para inicializar un equipo
    subroutine inicializarEquipo(this, descripcion, stock, precio, almacen)
        class(equipo), intent(inout) :: this
        character(len=256), intent(in) :: descripcion
        integer, intent(in) :: stock
        real, intent(in) :: precio
        character(len=256), intent(in) :: almacen
        this%descripcion = descripcion
        this%stock = stock
        this%precio = precio
        this%almacen = almacen  
    end subroutine inicializarEquipo 

    ! Subrutina para aumentar el stock
    subroutine aumentarStock(this, stock)
        class(equipo), intent(inout) :: this
        integer, intent(in) :: stock
        this%stock = this%stock + stock
    end subroutine aumentarStock

    ! Subrutina para disminuir el stock
    subroutine disminuirStock(this, stock)
        class(equipo), intent(inout) :: this
        integer, intent(in) :: stock
        this%stock = this%stock - stock
    end subroutine disminuirStock 

end module equipoModule


module globalVars
    use equipoModule
    integer :: contador = 1
    integer :: i
    type(equipo), dimension(150) :: inventario  
end module globalVars


program sistemaInventario
    use equipoModule
    use globalVars
    implicit none
    integer :: opcion

    do
        print *, '****************************************'
        print *, '         SISTEMA DE INVENTARIO          '
        print *, '****************************************'
        print *, '1. Cargar Inventario Inicial'
        print *, '2. Cargar Instrucciones de Movimientos'
        print *, '3. Crear Informe de Inventario'
        print *, '4. Salir'
        print *, '****************************************'
        print *, 'Seleccione una opcion (1-4):'
        print *, '****************************************'
        read(*, *) opcion

        select case (opcion)
            case (1)
                call cargarInventario()
            case (2)
                call ejecutarAcciones()
            case (3)
                call generarInforme()
            case (4)
                print *, "Saliendo del sistema..."
                do i = 1, contador-1
                    print *, "Equipo: ", i
                    print *, "Nombre: ", inventario(i)%descripcion
                    print *, "Stock: ", inventario(i)%stock
                    print *, "Precio: ", inventario(i)%precio
                    print *, "Ubicación: ", inventario(i)%almacen
                end do
                stop
            case default 
                print *, "Opción no valida"
        end select
    end do

end program sistemaInventario


subroutine cargarInventario()
    use equipoModule
    use globalVars
    integer :: archivoUnidad, estadoIO, posicion, stockInt
    real :: precioReal
    character(len=256) :: descripcion, stock, precio, almacen, linea, comando, rutaArchivo

    print *, "****************************************"
    print *, "*   Ingrese la ruta del archivo de     *"
    print *, "*         inventario inicial:          *"
    print *, "****************************************"
    read *, rutaArchivo  

    archivoUnidad = 10
    open(unit=archivoUnidad, file=trim(rutaArchivo), status="old", action="read", iostat=estadoIO)
    if (estadoIO /= 0) then
        print *, "Error al abrir el archivo: ", rutaArchivo
        stop    
    end if

    do 
        read(archivoUnidad, '(A)', iostat=estadoIO) linea
        if (estadoIO /= 0) exit
        linea = trim(linea)
        
        posicion = index(linea, ' ')
        if (posicion > 0) then
            comando = linea(1:posicion-1)
            linea = trim(linea(posicion+1:))
            posicion = index(linea, ';')
            if (posicion > 0) then
                descripcion = linea(1:posicion-1)
                linea = trim(linea(posicion+1:))
                posicion = index(linea, ';')
                if (posicion > 0) then
                    stock = linea(1:posicion-1)
                    linea = trim(linea(posicion+1:))
                    read(stock, '(I10)', iostat=estadoIO) stockInt
                    posicion = index(linea, ';')
                    if (posicion > 0) then
                        precio = linea(1:posicion-1)
                        read(precio, '(F10.2)', iostat=estadoIO) precioReal
                        almacen = trim(linea(posicion+1:))

                        if (comando == "crear_equipo") then
                            call registrarEquipo(descripcion, stockInt, precioReal, almacen)
                            print *, "****************************************"
                            print *, "Nombre: ", trim(descripcion)
                            print *, "Stock: ", stockInt
                            print *, "Precio: ", precioReal
                            print *, "Ubicacion: ", trim(almacen)
                            print *, "****************************************"
                        end if

                    end if
                end if
            end if
        end if
    end do
    close(unit=archivoUnidad)
end subroutine cargarInventario


subroutine registrarEquipo(descripcion, stock, precio, almacen)
    use equipoModule
    use globalVars
    character(len=256), intent(in) :: descripcion
    integer, intent(in) :: stock
    real, intent(in) :: precio
    character(len=256), intent(in) :: almacen
    type(equipo) :: nuevoEquipo

    call nuevoEquipo%inicializarEquipo(descripcion, stock, precio, almacen)
    inventario(contador) = nuevoEquipo
    contador = contador + 1
end subroutine registrarEquipo


subroutine ejecutarAcciones()
    use equipoModule
    use globalVars
    integer :: archivoUnidad, estadoIO, posicion, stockInt
    character(len=256) :: descripcion, almacen, linea, comando, rutaArchivoAcciones

    print *, "****************************************"
    print *, "*   Ingrese la ruta del archivo de     *"
    print *, "*    instrucciones de movimientos:     *"
    print *, "****************************************"
    read *, rutaArchivoAcciones

    archivoUnidad = 11
    open(unit=archivoUnidad, file=trim(rutaArchivoAcciones), status="old", action="read", iostat=estadoIO)
    if (estadoIO /= 0) then
        print *, "Error al abrir el archivo: ", trim(rutaArchivoAcciones)
        stop
    end if

    print *, "****************************************"
    print *, "*  Procesando archivo de acciones:     *"
    print *, "*   ", trim(rutaArchivoAcciones), "    *"
    print *, "****************************************"

    do
        read(archivoUnidad, '(A)', iostat=estadoIO) linea
        if (estadoIO /= 0) exit
        linea = trim(linea)
        
        print *, "----------------------------------------"
        print *, " Leyendo linea: ", trim(adjustl(linea))
        print *, "----------------------------------------"

        posicion = index(linea, ' ')
        if (posicion > 0) then
            comando = trim(linea(1:posicion-1))
            linea = trim(linea(posicion+1:))
            posicion = index(linea, ';')
            if (posicion > 0) then
                descripcion = trim(linea(1:posicion-1))
                linea = trim(linea(posicion+1:))
                posicion = index(linea, ';')
                if (posicion > 0) then
                    read(linea(1:posicion-1), '(I10)', iostat=estadoIO) stockInt
                    almacen = trim(linea(posicion+1:))

                    if (comando == "agregar_stock") then
                        call aumentar_stock(descripcion, stockInt, almacen)
                    else if (comando == "eliminar_equipo") then
                        call disminuir_stock(descripcion, stockInt, almacen)
                    end if
                end if
            end if
        end if
    end do

    print *, "****************************************"
    print *, "*       Procesamiento completado       *"
    print *, "****************************************"

    close(unit=archivoUnidad)
end subroutine ejecutarAcciones


subroutine aumentar_stock(descripcion, stock, almacen)
    use equipoModule
    use globalVars
    character(len=256), intent(in) :: descripcion
    integer, intent(in) :: stock
    character(len=256), intent(in) :: almacen
    logical :: encontrado = .false.

    do i = 1, contador-1
        if (trim(inventario(i)%descripcion) == trim(descripcion) .and. trim(inventario(i)%almacen) == trim(almacen)) then
            call inventario(i)%aumentarStock(stock)
            encontrado = .true.
            print *, "Stock agregado a ", trim(descripcion), " en ", trim(almacen)
            print *, "Nuevo stock: ", inventario(i)%stock
        end if
    end do

    if (.not. encontrado) then
        print *, "No se encontro el producto ", trim(descripcion), " en la ubicacion ", trim(almacen)
    end if
end subroutine aumentar_stock


subroutine disminuir_stock(descripcion, stock, almacen)
    use equipoModule
    use globalVars
    character(len=256), intent(in) :: descripcion
    integer, intent(in) :: stock
    character(len=256), intent(in) :: almacen
    logical :: encontrado = .false.

    do i = 1, contador-1
        if (trim(inventario(i)%descripcion) == trim(descripcion) .and. trim(inventario(i)%almacen) == trim(almacen)) then
            encontrado = .true.
            if (stock <= inventario(i)%stock) then
                call inventario(i)%disminuirStock(stock)
                print *, "Stock reducido de ", trim(descripcion), " en ", trim(almacen)
                print *, "Nuevo stock: ", inventario(i)%stock
            else
                print *, "Error: La cantidad a eliminar es mayor que la cantidad existente en ", trim(almacen)
            end if
        end if
    end do

    if (.not. encontrado) then
        print *, "No se encontro el producto ", trim(descripcion), " en la ubicación ", trim(almacen)
    end if
end subroutine disminuir_stock


subroutine generarInforme()
    use equipoModule
    use globalVars
    implicit none
    integer :: archivoUnidad, j
    real :: valorTotal
    character(len=256) :: archivoSalida

    archivoSalida = 'informe.txt'
    archivoUnidad = 20

    open(unit=archivoUnidad, file=trim(archivoSalida), status='replace', action='write')

    write(archivoUnidad, '(A)') '********************************************************************************'
    write(archivoUnidad, '(A)') '*                        Informe de Inventario Actual                          *'
    write(archivoUnidad, '(A)') '********************************************************************************'
    write(archivoUnidad, '(A)') '* Equipo               | Cantidad | Precio Unit | Valor Total | Ubicación      *'
    write(archivoUnidad, '(A)') '--------------------------------------------------------------------------------'

    do j = 1, contador-1
        valorTotal = inventario(j)%stock * inventario(j)%precio
        write(archivoUnidad, '(A, T22, I7, T32, F11.2, T45, F12.2, T60, A)') &
            trim(adjustl(inventario(j)%descripcion)), inventario(j)%stock, inventario(j)%precio, valorTotal, trim(adjustl(inventario(j)%almacen))
    end do

    close(archivoUnidad)

    print *, "Informe de inventario creado en", trim(archivoSalida)
end subroutine generarInforme
