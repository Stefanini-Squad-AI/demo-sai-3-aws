package com.card.management.Services;

import com.card.management.DTOs.AccountUpdateRequestDto;
import com.card.management.Models.Account;
import com.card.management.Models.Customer;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.CustomerRepository;
import com.card.management.Repositories.CardXrefRecordRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AccountUpdateService {
  @Autowired
  private AccountRepository accountRepository;

  @Autowired
  private CustomerRepository customerRepository;

  @Autowired
  private CardXrefRecordRepository cardXrefRecordRepository;

  /**
   * Obtiene datos de cuenta con información del cliente
   * Equivalente a 9000-READ-ACCT, 9300-GETACCTDATA-BYACCT,
   * 9400-GETCUSTDATA-BYCUST
   * 
   * Busca el customer a través de la tabla intermedia CardXrefRecord
   */
  public AccountUpdateRequestDto getAccountWithCustomer(Long accountId) {
    Account account = accountRepository.findByAccountIdOptional(accountId)
        .orElseThrow(() -> new RuntimeException("Account not found in account master file"));

    // OPCIÓN 1: Buscar cliente usando query directo en CustomerRepository
    Customer customer = customerRepository.findByAccountId(accountId)
        .orElseThrow(() -> new RuntimeException("Associated customer not found in master file"));

    return mapToAccountUpdateRequest(account, customer);
  }

  /**
   * Método alternativo que usa el repositorio CardXrefRecord explícitamente
   * para mayor claridad en la lógica de negocio
   */
  public AccountUpdateRequestDto getAccountWithCustomerExplicit(Long accountId) {
    Account account = accountRepository.findByAccountIdOptional(accountId)
        .orElseThrow(() -> new RuntimeException("Account not found in account master file"));

    // OPCIÓN 2: Buscar usando CardXrefRecord explícitamente
    CardXrefRecord cardXref = cardXrefRecordRepository.findByAccountId(accountId)
        .orElseThrow(() -> new RuntimeException("No card cross-reference found for account"));

    Customer customer = customerRepository.findByCustomerId(cardXref.getCustomerId())
        .orElseThrow(() -> new RuntimeException("Associated customer not found in master file"));

    return mapToAccountUpdateRequest(account, customer);
  }

  /**
   * Actualiza cuenta y datos del cliente
   * Equivalente a 9600-WRITE-PROCESSING con manejo transaccional
   */
  @Transactional
  public void updateAccountAndCustomer(AccountUpdateRequestDto request) {
    // Leer para actualización (equivalente a READ UPDATE en COBOL)
    Account account = accountRepository.findByAccountIdOptional(request.getAccountId())
        .orElseThrow(() -> new RuntimeException("Could not lock account record for update"));

    Customer customer = customerRepository.findByCustomerId(request.getCustomerId())
        .orElseThrow(() -> new RuntimeException("Could not lock customer record for update"));

    // Verificar si los datos cambiaron (equivalente a 9700-CHECK-CHANGE-IN-REC)
    // Esta lógica se implementaría comparando versiones o timestamps

    // Actualizar entidades
    updateAccountFromRequest(account, request);
    updateCustomerFromRequest(customer, request);

    // Guardar cambios (equivalente a REWRITE en COBOL)
    accountRepository.save(account);
    customerRepository.save(customer);
  }

  private AccountUpdateRequestDto mapToAccountUpdateRequest(Account account, Customer customer) {
    AccountUpdateRequestDto request = new AccountUpdateRequestDto();

    // Mapear datos de cuenta
    request.setAccountId(account.getAccountId());
    request.setActiveStatus(account.getActiveStatus());
    request.setCurrentBalance(account.getCurrentBalance());
    request.setCreditLimit(account.getCreditLimit());
    request.setCashCreditLimit(account.getCashCreditLimit());
    request.setOpenDate(account.getOpenDate());
    request.setExpirationDate(account.getExpirationDate());
    request.setReissueDate(account.getReissueDate());
    request.setCurrentCycleCredit(account.getCurrentCycleCredit());
    request.setCurrentCycleDebit(account.getCurrentCycleDebit());
    request.setGroupId(account.getGroupId());

    // Mapear datos de cliente
    request.setCustomerId(customer.getCustomerId());
    request.setFirstName(customer.getFirstName());
    request.setMiddleName(customer.getMiddleName());
    request.setLastName(customer.getLastName());
    request.setAddressLine1(customer.getAddressLine1());
    request.setAddressLine2(customer.getAddressLine2());
    request.setAddressLine3(customer.getAddressLine3());
    request.setStateCode(customer.getStateCode());
    request.setCountryCode(customer.getCountryCode());
    request.setZipCode(customer.getZipCode());
    request.setPhoneNumber1(customer.getPhoneNumber1());
    request.setPhoneNumber2(customer.getPhoneNumber2());
    request.setSsn(customer.getSocialSecurityNumber());
    request.setGovernmentIssuedId(customer.getGovernmentIssuedId());
    request.setDateOfBirth(customer.getDateOfBirth());
    request.setEftAccountId(customer.getEftAccountId());
    request.setPrimaryCardIndicator(customer.getPrimaryCardHolderIndicator());
    request.setFicoScore(customer.getFicoScore());

    return request;
  }

  private void updateAccountFromRequest(Account account, AccountUpdateRequestDto request) {
    account.setActiveStatus(request.getActiveStatus());
    account.setCurrentBalance(request.getCurrentBalance());
    account.setCreditLimit(request.getCreditLimit());
    account.setCashCreditLimit(request.getCashCreditLimit());
    account.setOpenDate(request.getOpenDate());
    account.setExpirationDate(request.getExpirationDate());
    account.setReissueDate(request.getReissueDate());
    account.setCurrentCycleCredit(request.getCurrentCycleCredit());
    account.setCurrentCycleDebit(request.getCurrentCycleDebit());
    account.setGroupId(request.getGroupId());
  }

  private void updateCustomerFromRequest(Customer customer, AccountUpdateRequestDto request) {
    customer.setFirstName(request.getFirstName());
    customer.setMiddleName(request.getMiddleName());
    customer.setLastName(request.getLastName());
    customer.setAddressLine1(request.getAddressLine1());
    customer.setAddressLine2(request.getAddressLine2());
    customer.setAddressLine3(request.getAddressLine3());
    customer.setStateCode(request.getStateCode());
    customer.setCountryCode(request.getCountryCode());
    customer.setZipCode(request.getZipCode());
    customer.setPhoneNumber1(request.getPhoneNumber1());
    customer.setPhoneNumber2(request.getPhoneNumber2());
    customer.setSocialSecurityNumber(request.getSsn());
    customer.setGovernmentIssuedId(request.getGovernmentIssuedId());
    customer.setDateOfBirth(request.getDateOfBirth());
    customer.setEftAccountId(request.getEftAccountId());
    customer.setPrimaryCardHolderIndicator(request.getPrimaryCardIndicator());
    customer.setFicoScore(request.getFicoScore());
  }
}
