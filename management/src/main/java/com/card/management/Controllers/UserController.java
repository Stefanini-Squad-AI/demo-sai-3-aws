package com.card.management.Controllers;

import com.card.management.DTOs.CreateUserResponseDto;
import com.card.management.DTOs.UserDeleteResponseDto;
import com.card.management.DTOs.CreateUserRequestDto;
import com.card.management.DTOs.UserListRequestDto;
import com.card.management.DTOs.UserListResponseDto;
import com.card.management.DTOs.UserUpdateDto;
import com.card.management.Models.User;
import com.card.management.Services.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.prepost.PreAuthorize;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

// ✅ NUEVO: Importar Map para la respuesta envuelta
import java.util.Map;
import java.util.HashMap;

@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "User Management", description = "User listing and management operations (ADMIN only)")
@PreAuthorize("hasRole('ADMIN')") // Todo el controlador requiere rol ADMIN
public class UserController {
  private final UserService userService;

  /**
   * Lista usuarios con paginación
   * Equivalente al programa principal COUSR00C en COBOL
   */
  @GetMapping("/list")
  @Operation(summary = "List users with pagination", description = "Retrieves a paginated list of users from USRSEC file")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Users retrieved successfully"),
      @ApiResponse(responseCode = "400", description = "Invalid request parameters"),
      @ApiResponse(responseCode = "500", description = "Internal server error")
  })
  public ResponseEntity<UserListResponseDto> listUsers(
      @Parameter(description = "Starting user ID for pagination") @RequestParam(required = false) String startUserId,

      @Parameter(description = "Page number") @RequestParam(defaultValue = "1") int pageNumber,

      @Parameter(description = "Navigation direction: FORWARD or BACKWARD") @RequestParam(defaultValue = "FORWARD") String direction) {

    UserListRequestDto request = new UserListRequestDto();
    request.setStartUserId(startUserId);
    request.setPageNumber(pageNumber);
    request.setDirection(direction);

    UserListResponseDto response = userService.processUserList(request);

    return ResponseEntity.ok(response);
  }

  /**
   * Procesa selección de usuario para actualización o eliminación
   * Equivalente a PROCESS-ENTER-KEY en COBOL
   */
  @PostMapping("/process-selection")
  @Operation(summary = "Process user selection", description = "Processes user selection for update (U) or delete (D) operations")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Selection processed successfully"),
      @ApiResponse(responseCode = "400", description = "Invalid selection parameters"),
      @ApiResponse(responseCode = "404", description = "User not found")
  })
  public ResponseEntity<String> processUserSelection(
      @RequestBody UserListRequestDto request) {

    // Validar selección
    if (!isValidSelection(request.getSelectionFlag())) {
      return ResponseEntity.badRequest()
          .body("Invalid selection. Valid values are U and D");
    }

    // Procesar según el tipo de selección
    String action = request.getSelectionFlag().toUpperCase();
    switch (action) {
      case "U":
        return ResponseEntity.ok("Redirecting to user update: " + request.getSelectedUserId());
      case "D":
        return ResponseEntity.ok("Redirecting to user delete: " + request.getSelectedUserId());
      default:
        return ResponseEntity.badRequest()
            .body("Invalid selection. Valid values are U and D");
    }
  }

  /**
   * Navega a la página anterior
   * Equivalente a PROCESS-PF7-KEY en COBOL
   */
  @GetMapping("/previous-page")
  @Operation(summary = "Navigate to previous page", description = "Navigates to the previous page of users")
  public ResponseEntity<UserListResponseDto> previousPage(
      @RequestParam String firstUserId,
      @RequestParam int currentPage) {

    if (currentPage <= 1) {
      UserListResponseDto response = new UserListResponseDto();
      response.setMessage("You are already at the top of the page...");
      return ResponseEntity.ok(response);
    }

    UserListRequestDto request = new UserListRequestDto();
    request.setStartUserId(firstUserId);
    request.setPageNumber(currentPage - 1);
    request.setDirection("BACKWARD");

    UserListResponseDto response = userService.processUserList(request);
    return ResponseEntity.ok(response);
  }

  /**
   * Navega a la página siguiente
   * Equivalente a PROCESS-PF8-KEY en COBOL
   */
  @GetMapping("/next-page")
  @Operation(summary = "Navigate to next page", description = "Navigates to the next page of users")
  public ResponseEntity<UserListResponseDto> nextPage(
      @RequestParam String lastUserId,
      @RequestParam int currentPage,
      @RequestParam boolean hasNextPage) {

    if (!hasNextPage) {
      UserListResponseDto response = new UserListResponseDto();
      response.setMessage("You are already at the bottom of the page...");
      return ResponseEntity.ok(response);
    }

    UserListRequestDto request = new UserListRequestDto();
    request.setStartUserId(lastUserId);
    request.setPageNumber(currentPage + 1);
    request.setDirection("FORWARD");

    UserListResponseDto response = userService.processUserList(request);
    return ResponseEntity.ok(response);
  }

  /**
   * Valida si la selección es válida (U o D)
   * Equivalente a la validación en PROCESS-ENTER-KEY
   */
  private boolean isValidSelection(String selectionFlag) {
    return selectionFlag != null &&
        (selectionFlag.equalsIgnoreCase("U") || selectionFlag.equalsIgnoreCase("D"));
  }

  /**
   * ✅ CORRECCIÓN: Crea un nuevo usuario en el sistema
   * SOLO ESTE MÉTODO se adapta al formato ApiResponse que espera el frontend
   * Migrado desde la funcionalidad principal del programa COUSR01C
   */
  @PostMapping
  @Operation(summary = "Crear nuevo usuario", description = "Crea un nuevo usuario regular o administrador en el sistema")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "201", description = "Usuario creado exitosamente"),
      @ApiResponse(responseCode = "400", description = "Datos de entrada inválidos"),
      @ApiResponse(responseCode = "409", description = "El usuario ya existe"),
      @ApiResponse(responseCode = "500", description = "Error interno del servidor")
  })
  public ResponseEntity<Map<String, Object>> createUser(@Valid @RequestBody CreateUserRequestDto request) {
    log.info("Solicitud de creación de usuario recibida para userId: {}", request.getUserId());

    CreateUserResponseDto serviceResponse = userService.createUser(request);

    // ✅ CORRECCIÓN: Envolver en formato ApiResponse que espera el frontend
    Map<String, Object> apiResponse = new HashMap<>();

    if (serviceResponse.isSuccess()) {
      // Caso exitoso: { success: true, data: CreateUserResponseDto }
      apiResponse.put("success", true);
      apiResponse.put("data", serviceResponse);
      return ResponseEntity.status(HttpStatus.CREATED).body(apiResponse);
    } else {
      // Caso de error: { success: false, error: "mensaje" }
      apiResponse.put("success", false);
      apiResponse.put("error", serviceResponse.getMessage());

      // Determinar el código de estado basado en el mensaje de error
      if (serviceResponse.getMessage().contains("already exist")) {
        return ResponseEntity.status(HttpStatus.CONFLICT).body(apiResponse);
      } else {
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(apiResponse);
      }
    }
  }

  /**
   * Obtiene información de un usuario por ID
   * Migrado desde PROCESS-ENTER-KEY y READ-USER-SEC-FILE
   */
  @GetMapping("/{userId}")
  @Operation(summary = "Obtener usuario por ID", description = "Busca y retorna la información de un usuario específico")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Usuario encontrado exitosamente"),
      @ApiResponse(responseCode = "404", description = "Usuario no encontrado")
  })
  public ResponseEntity<User> getUserById(
      @Parameter(description = "ID del usuario a buscar", example = "USER001") @PathVariable String userId) {

    log.debug("Solicitud para obtener usuario: {}", userId);
    User user = userService.findUserById(userId);
    return ResponseEntity.ok(user);
  }

  /**
   * Actualiza información de un usuario
   * Migrado desde UPDATE-USER-INFO y las validaciones correspondientes
   */
  @PutMapping("/{userId}")
  @Operation(summary = "Actualizar usuario", description = "Actualiza la información de un usuario existente")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Usuario actualizado exitosamente"),
      @ApiResponse(responseCode = "400", description = "Datos de entrada inválidos"),
      @ApiResponse(responseCode = "404", description = "Usuario no encontrado")
  })
  public ResponseEntity<User> updateUser(
      @Parameter(description = "ID del usuario a actualizar", example = "USER001") @PathVariable String userId,
      @Valid @RequestBody UserUpdateDto userUpdateDto) {

    log.debug("Solicitud para actualizar usuario: {}", userId);

    // Validar que el userId del path coincida con el del DTO
    if (!userId.equals(userUpdateDto.getUserId())) {
      throw new IllegalArgumentException("User ID in path must match User ID in request body");
    }

    User updatedUser = userService.updateUser(userId, userUpdateDto);
    return ResponseEntity.ok(updatedUser);
  }

  /**
   * Elimina el usuario del sistema
   * Equivalente a DELETE-USER-INFO en COBOL
   */
  @DeleteMapping("/{userId}")
  @Operation(summary = "Eliminar usuario", description = "Elimina permanentemente el usuario del sistema")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Usuario eliminado exitosamente"),
      @ApiResponse(responseCode = "404", description = "Usuario no encontrado"),
      @ApiResponse(responseCode = "400", description = "ID de usuario inválido"),
      @ApiResponse(responseCode = "500", description = "Error interno del servidor")
  })
  public ResponseEntity<UserDeleteResponseDto> deleteUser(
      @Parameter(description = "ID del usuario a eliminar", required = true) @PathVariable String userId) {

    log.info("Solicitando eliminación de usuario: {}", userId);

    UserDeleteResponseDto response = userService.deleteUser(userId);

    if (!response.isSuccess()) {
      if (response.getMessage().contains("NOT found")) {
        return ResponseEntity.notFound().build();
      }
      if (response.getMessage().contains("empty")) {
        return ResponseEntity.badRequest().body(response);
      }
      return ResponseEntity.internalServerError().body(response);
    }

    return ResponseEntity.ok(response);
  }
}