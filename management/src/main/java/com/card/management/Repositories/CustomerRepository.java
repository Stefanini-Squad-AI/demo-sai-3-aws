package com.card.management.Repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import com.card.management.Models.Customer;

public interface CustomerRepository extends JpaRepository<Customer, Long> {
  // Métodos de consulta personalizados si es necesario
  @Query("SELECT c FROM Customer c WHERE c.customerId = :customerId")
  Optional<Customer> findByCustomerId(@Param("customerId") Long customerId);

  // Buscar customer por accountId a través de CardXrefRecord (tabla intermedia)
  @Query("SELECT c FROM Customer c " +
         "JOIN CardXrefRecord cxr ON c.customerId = cxr.customerId " +
         "WHERE cxr.accountId = :accountId")
  Optional<Customer> findByAccountId(@Param("accountId") Long accountId);
}
