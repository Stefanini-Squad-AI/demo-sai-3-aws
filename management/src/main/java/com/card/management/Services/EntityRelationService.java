package com.card.management.Services;

import com.card.management.Models.Account;
import com.card.management.Models.DisclosureGroup;
import com.card.management.Models.TransactionCategory;
import com.card.management.Models.TransactionType;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.DisclosureGroupRepository;
import com.card.management.Repositories.TransactionCategoryRepository;
import com.card.management.Repositories.TransactionTypeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

/**
 * Servicio para manejar las relaciones entre entidades sin mapeos JPA conflictivos
 */
@Service
public class EntityRelationService {

    @Autowired
    private AccountRepository accountRepository;
    
    @Autowired
    private DisclosureGroupRepository disclosureGroupRepository;
    
    @Autowired
    private TransactionTypeRepository transactionTypeRepository;
    
    @Autowired
    private TransactionCategoryRepository transactionCategoryRepository;

    /**
     * Obtiene todos los grupos de divulgación para una cuenta específica
     * @param accountId ID de la cuenta
     * @return Lista de grupos de divulgación asociados
     */
    public List<DisclosureGroup> getDisclosureGroupsByAccount(Long accountId) {
        Optional<Account> account = accountRepository.findById(accountId);
        if (account.isPresent() && account.get().getGroupId() != null) {
            return disclosureGroupRepository.findByAccountGroupId(account.get().getGroupId());
        }
        return List.of();
    }

    /**
     * Obtiene la cuenta asociada a un grupo de divulgación
     * @param disclosureGroup Grupo de divulgación
     * @return Account asociada si existe
     */
    public Optional<Account> getAccountByDisclosureGroup(DisclosureGroup disclosureGroup) {
        return accountRepository.findByGroupId(disclosureGroup.getAccountGroupId());
    }

    /**
     * Obtiene todas las categorías de transacción para un tipo específico
     * @param transactionTypeCode Código del tipo de transacción
     * @return Lista de categorías asociadas
     */
    public List<TransactionCategory> getCategoriesByTransactionType(String transactionTypeCode) {
        return transactionCategoryRepository.findByTransactionTypeCode(transactionTypeCode);
    }

    /**
     * Obtiene el tipo de transacción para una categoría específica
     * @param transactionCategory Categoría de transacción
     * @return TransactionType asociado si existe
     */
    public Optional<TransactionType> getTransactionTypeByCategory(TransactionCategory transactionCategory) {
        return transactionTypeRepository.findById(transactionCategory.getTransactionTypeCode());
    }
}