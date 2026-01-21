package com.card.management.Services;

import com.card.management.DTOs.UserListRequestDto;
import com.card.management.DTOs.UserListResponseDto;
import com.card.management.DTOs.UserUpdateDto;
import com.card.management.DTOs.CreateUserRequestDto;
import com.card.management.DTOs.CreateUserResponseDto;
import com.card.management.DTOs.UserDeleteResponseDto;
import com.card.management.DTOs.UserDto;
import com.card.management.DTOs.UserListItemDto;
import com.card.management.Models.User;
import com.card.management.Repositories.UserRepository;
import com.card.management.Exceptions.UserNotFoundException;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class UserService {
  private final PasswordEncoder passwordEncoder;
  private final UserRepository userRepository;
  private static final int PAGE_SIZE = 10; // Equivalente a OCCURS 10 TIMES en COBOL

  /**
   * Procesa la solicitud de listado de usuarios con paginación
   * Equivalente a PROCESS-PAGE-FORWARD y PROCESS-PAGE-BACKWARD en COBOL
   */
  public UserListResponseDto processUserList(UserListRequestDto request) {
    try {
      Pageable pageable = PageRequest.of(0, PAGE_SIZE);
      Page<User> userPage;

      // Determinar dirección de navegación
      if ("BACKWARD".equals(request.getDirection())) {
        userPage = processPageBackward(request.getStartUserId(), pageable);
      } else {
        userPage = processPageForward(request.getStartUserId(), pageable);
      }

      List<UserListItemDto> userItems = userPage.getContent().stream()
          .map(this::convertToUserListItem)
          .collect(Collectors.toList());

      // Verificar si hay página siguiente
      boolean hasNextPage = checkHasNextPage(userPage, request.getDirection());

      UserListResponseDto response = new UserListResponseDto();
      response.setUsers(userItems);
      response.setPageNumber(request.getPageNumber());
      response.setHasNextPage(hasNextPage);

      // Establecer primer y último usuario ID para navegación
      if (!userItems.isEmpty()) {
        response.setFirstUserId(userItems.get(0).getUserId());
        response.setLastUserId(userItems.get(userItems.size() - 1).getUserId());
      }

      return response;

    } catch (Exception e) {
      UserListResponseDto errorResponse = new UserListResponseDto();
      errorResponse.setMessage("Unable to lookup User...");
      return errorResponse;
    }
  }

  /**
   * Equivalente a PROCESS-PAGE-FORWARD en COBOL
   */
  private Page<User> processPageForward(String startUserId, Pageable pageable) {
    if (StringUtils.hasText(startUserId)) {
      return userRepository.findUsersFromId(startUserId, pageable);
    } else {
      return userRepository.findAllUsersOrdered(pageable);
    }
  }

  /**
   * Equivalente a PROCESS-PAGE-BACKWARD en COBOL
   */
  private Page<User> processPageBackward(String startUserId, Pageable pageable) {
    if (StringUtils.hasText(startUserId)) {
      Page<User> reversePage = userRepository.findUsersBeforeId(startUserId, pageable);
      // Invertir el orden para mostrar correctamente
      List<User> reversedContent = reversePage.getContent().stream()
          .sorted((u1, u2) -> u1.getUserId().compareTo(u2.getUserId()))
          .collect(Collectors.toList());
      return new org.springframework.data.domain.PageImpl<>(
          reversedContent, pageable, reversePage.getTotalElements());
    } else {
      return userRepository.findAllUsersOrdered(pageable);
    }
  }

  /**
   * Verifica si hay página siguiente disponible
   * Equivalente a la lógica de NEXT-PAGE-FLG en COBOL
   */
  private boolean checkHasNextPage(Page<User> currentPage, String direction) {
    if ("BACKWARD".equals(direction)) {
      return currentPage.hasPrevious();
    } else {
      return currentPage.hasNext();
    }
  }

  /**
   * Convierte User entity a UserListItemDto
   * Equivalente a POPULATE-USER-DATA en COBOL
   */
  private UserListItemDto convertToUserListItem(User user) {
    UserListItemDto item = new UserListItemDto();
    item.setUserId(user.getUserId());
    item.setFirstName(user.getFirstName());
    item.setLastName(user.getLastName());
    item.setUserType(user.getUserType());
    return item;
  }

  /**
   * Crea un nuevo usuario en el sistema
   * Migrado desde el párrafo WRITE-USER-SEC-FILE del programa COBOL
   */
  @Transactional
  public CreateUserResponseDto createUser(CreateUserRequestDto request) {
    try {
      // Verificar si el usuario ya existe (equivalente a DFHRESP(DUPKEY))
      if (userRepository.existsByUserId(request.getUserId())) {
        log.warn("Intento de crear usuario duplicado: {}", request.getUserId());
        return new CreateUserResponseDto(false, "User ID already exist...", null);
      }

      // Crear nueva entidad usuario
      User user = new User();
      user.setUserId(request.getUserId());
      user.setFirstName(request.getFirstName());
      user.setLastName(request.getLastName());
      user.setPassword(passwordEncoder.encode(request.getPassword()));
      user.setUserType(request.getUserType());

      // Guardar usuario (equivalente a CICS WRITE)
      User savedUser = userRepository.save(user);

      // Crear DTO de respuesta
      UserDto userDto = new UserDto();
      userDto.setUserId(savedUser.getUserId());
      userDto.setFirstName(savedUser.getFirstName());
      userDto.setLastName(savedUser.getLastName());
      userDto.setUserType(savedUser.getUserType());

      String successMessage = String.format("User %s has been added ...", savedUser.getUserId());
      log.info("Usuario creado exitosamente: {}", savedUser.getUserId());

      return new CreateUserResponseDto(true, successMessage, userDto);

    } catch (Exception e) {
      log.error("Error al crear usuario: {}", e.getMessage(), e);
      return new CreateUserResponseDto(false, "Unable to Add User...", null);
    }
  }

  /**
   * Busca un usuario por ID
   * Migrado desde READ-USER-SEC-FILE
   */
  public User findUserById(String userId) {
    log.debug("Buscando usuario con ID: {}", userId);
    return userRepository.findById(userId)
        .orElseThrow(() -> new UserNotFoundException("User ID NOT found..."));
  }

  /**
   * Actualiza la información de un usuario
   * Migrado desde UPDATE-USER-INFO y UPDATE-USER-SEC-FILE
   */
  @Transactional
  public User updateUser(String userId, UserUpdateDto userUpdateDto) {
    log.debug("Actualizando usuario con ID: {}", userId);

    // Buscar usuario existente (equivalente a READ con UPDATE)
    User existingUser = findUserById(userId);

    // Verificar si hay cambios (equivalente a la lógica USR-MODIFIED)
    boolean userModified = false;

    if (!userUpdateDto.getFirstName().equals(existingUser.getFirstName())) {
      existingUser.setFirstName(userUpdateDto.getFirstName());
      userModified = true;
    }

    if (!userUpdateDto.getLastName().equals(existingUser.getLastName())) {
      existingUser.setLastName(userUpdateDto.getLastName());
      userModified = true;
    }

    if (!userUpdateDto.getPassword().equals(existingUser.getPassword())) {
      existingUser.setPassword(userUpdateDto.getPassword());
      userModified = true;
    }

    if (!userUpdateDto.getUserType().equals(existingUser.getUserType())) {
      existingUser.setUserType(userUpdateDto.getUserType());
      userModified = true;
    }

    if (!userModified) {
      throw new IllegalStateException("Please modify to update ...");
    }

    // Guardar cambios (equivalente a CICS REWRITE)
    User updatedUser = userRepository.save(existingUser);
    log.info("Usuario {} ha sido actualizado", userId);

    return updatedUser;
  }

  /**
   * Elimina el usuario del sistema
   * Equivalente a DELETE-USER-SEC-FILE en COBOL
   */
  @Transactional
  public UserDeleteResponseDto deleteUser(String userId) {
    log.info("Iniciando eliminación de usuario: {}", userId);

    if (userId == null || userId.trim().isEmpty()) {
      return new UserDeleteResponseDto(null, "User ID can NOT be empty...", false);
    }

    try {
      String trimmedUserId = userId.trim();

      if (!userRepository.existsById(trimmedUserId)) {
        log.warn("Usuario no encontrado para eliminación: {}", trimmedUserId);
        return new UserDeleteResponseDto(trimmedUserId, "User ID NOT found...", false);
      }

      userRepository.deleteById(trimmedUserId);
      log.info("Usuario eliminado exitosamente: {}", trimmedUserId);

      return new UserDeleteResponseDto(
          trimmedUserId,
          String.format("User %s has been deleted ...", trimmedUserId),
          true);

    } catch (Exception e) {
      log.error("Error al eliminar usuario {}: {}", userId, e.getMessage());
      return new UserDeleteResponseDto(userId, "Unable to Update User...", false);
    }
  }
}
