package com.card.management.Repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import com.card.management.Models.RejectedTransaction;

@Repository
public interface RejectedTransactionRepository extends JpaRepository<RejectedTransaction, Long> {
    // Métodos adicionales si son necesarios
}