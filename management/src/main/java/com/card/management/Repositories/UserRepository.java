package com.card.management.Repositories;

import com.card.management.Models.User;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRepository extends JpaRepository<User, String> {
  // Equivalente a STARTBR con GTEQ - buscar desde un ID específico hacia adelante
  @Query("SELECT u FROM User u WHERE u.userId >= :userId ORDER BY u.userId ASC")
  Page<User> findUsersFromId(@Param("userId") String userId, Pageable pageable);

  // Para navegación hacia atrás
  @Query("SELECT u FROM User u WHERE u.userId < :userId ORDER BY u.userId DESC")
  Page<User> findUsersBeforeId(@Param("userId") String userId, Pageable pageable);

  // Buscar todos los usuarios ordenados
  @Query("SELECT u FROM User u ORDER BY u.userId ASC")
  Page<User> findAllUsersOrdered(Pageable pageable);

  boolean existsByUserId(String userId);

  Optional<User> findByUserId(String userId);
  
}
