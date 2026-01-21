package com.card.management.Repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.card.management.Models.Account;

public interface AccountRepository extends JpaRepository<Account, Long> {
  // Métodos de consulta personalizados si es necesario
  @Query("SELECT a FROM Account a WHERE a.accountId = :accountId")
  Optional<Account> findByAccountIdOptional(@Param("accountId") Long accountId);

  // Spring Data JPA creará automáticamente esta consulta basada en el nombre del método
  Account findByAccountId(Long accountId);

  Optional<Account> findByGroupId(String groupId);
}
