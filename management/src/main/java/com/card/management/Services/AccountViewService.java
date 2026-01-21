package com.card.management.Services;

import com.card.management.DTOs.AccountViewRequestDto;
import com.card.management.DTOs.AccountViewResponseDto;
import com.card.management.DTOs.AccountViewResponseDto.AccountViewResponseDtoBuilder;
import com.card.management.Models.Account;
import com.card.management.Models.Customer;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.CustomerRepository;
import com.card.management.Repositories.CardXrefRecordRepository;
import com.card.management.Utils.ValidationUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

/**
 * Servicio para manejar la lógica de negocio de visualización de cuentas
 * Migrado desde COACTVWC.CBL
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class AccountViewService {
  private final AccountRepository accountRepository;
  private final CustomerRepository customerRepository;
  private final CardXrefRecordRepository cardXrefRepository;
  private final ValidationUtil validationUtil;

  /**
   * Procesa la solicitud de visualización de cuenta
   * Equivalente a la sección 2000-PROCESS-INPUTS y 9000-READ-ACCT
   */
  public AccountViewResponseDto processAccountViewRequest(AccountViewRequestDto request) {
    log.debug("Processing account view request: {}", request);

    // Validar entradas - equivalente a 2200-EDIT-MAP-INPUTS
    AccountViewResponseDto validationResponse = validateInputs(request);
    if (!validationResponse.isInputValid()) {
      return validationResponse;
    }

    // Leer datos de la cuenta - equivalente a 9000-READ-ACCT
    return readAccountData(request.getAccountId());
  }

  /**
   * Inicializa la pantalla de visualización
   * Equivalente a 1000-SEND-MAP cuando CDEMO-PGM-ENTER
   */
  public AccountViewResponseDto initializeAccountViewScreen() {
    return AccountViewResponseDto.builder()
        .currentDate(getCurrentDate())
        .currentTime(getCurrentTime())
        .transactionId("CAVW")
        .programName("COACTVWC")
        .infoMessage("Enter or update id of account to display")
        .inputValid(true)
        .build();
  }

  /**
   * Valida las entradas del usuario
   * Equivalente a 2200-EDIT-MAP-INPUTS y 2210-EDIT-ACCOUNT
   */
  private AccountViewResponseDto validateInputs(AccountViewRequestDto request) {
    AccountViewResponseDtoBuilder responseBuilder = AccountViewResponseDto.builder()
        .currentDate(getCurrentDate())
        .currentTime(getCurrentTime())
        .transactionId("CAVW")
        .programName("COACTVWC");

    // Validar que se proporcione ID de cuenta
    if (request.getAccountId() == null) {
      return responseBuilder
          .errorMessage("Account number not provided")
          .inputValid(false)
          .accountFilterValid(false)
          .build();
    }

    // Validar formato numérico y longitud - equivalente a validaciones COBOL
    if (!validationUtil.isValidAccountId(request.getAccountId())) {
      return responseBuilder
          .errorMessage("Account Filter must be a non-zero 11 digit number")
          .inputValid(false)
          .accountFilterValid(false)
          .build();
    }

    return responseBuilder
        .inputValid(true)
        .accountFilterValid(true)
        .build();
  }

  /**
   * Lee los datos de la cuenta desde múltiples fuentes
   * Equivalente a 9000-READ-ACCT, 9200-GETCARDXREF-BYACCT,
   * 9300-GETACCTDATA-BYACCT, 9400-GETCUSTDATA-BYCUST
   */
  public AccountViewResponseDto readAccountData(Long accountId) {
    AccountViewResponseDtoBuilder responseBuilder = AccountViewResponseDto.builder()
        .currentDate(getCurrentDate())
        .currentTime(getCurrentTime())
        .transactionId("CAVW")
        .programName("COACTVWC")
        .accountId(accountId);

    try {
      // 1. Buscar en CardXref por AccountId - equivalente a 9200-GETCARDXREF-BYACCT
      Optional<CardXrefRecord> cardXrefOpt = cardXrefRepository.findByAccountId(accountId);
      if (cardXrefOpt.isEmpty()) {
        return responseBuilder
            .errorMessage("Account:" + accountId + " not found in Cross ref file")
            .inputValid(false)
            .accountFilterValid(false)
            .build();
      }

      CardXrefRecord cardXref = cardXrefOpt.get();
      Long customerId = cardXref.getCustomerId();
      String cardNumber = cardXref.getCardNumber();

      // 2. Buscar datos de la cuenta - equivalente a 9300-GETACCTDATA-BYACCT
      Optional<Account> accountOpt = accountRepository.findByAccountIdOptional(accountId);
      if (accountOpt.isEmpty()) {
        return responseBuilder
            .errorMessage("Account:" + accountId + " not found in Acct Master file")
            .inputValid(false)
            .accountFilterValid(false)
            .build();
      }

      Account account = accountOpt.get();

      // 3. Buscar datos del cliente - equivalente a 9400-GETCUSTDATA-BYCUST
      Optional<Customer> customerOpt = customerRepository.findById(customerId);
      if (customerOpt.isEmpty()) {
        return responseBuilder
            .errorMessage("CustId:" + customerId + " not found in customer master")
            .inputValid(false)
            .customerFilterValid(false)
            .build();
      }

      Customer customer = customerOpt.get();

      // Construir respuesta con todos los datos - equivalente a
      // 1200-SETUP-SCREEN-VARS
      return responseBuilder
          .inputValid(true)
          .accountFilterValid(true)
          .customerFilterValid(true)
          .foundAccountInMaster(true)
          .foundCustomerInMaster(true)
          .customerId(customerId)
          .cardNumber(cardNumber)
          // Datos de la cuenta
          .accountStatus(account.getActiveStatus())
          .currentBalance(account.getCurrentBalance())
          .creditLimit(account.getCreditLimit())
          .cashCreditLimit(account.getCashCreditLimit())
          .currentCycleCredit(account.getCurrentCycleCredit())
          .currentCycleDebit(account.getCurrentCycleDebit())
          .openDate(account.getOpenDate())
          .expirationDate(account.getExpirationDate())
          .reissueDate(account.getReissueDate())
          .groupId(account.getGroupId())
          // Datos del cliente
          .customerSsn(formatSsn(customer.getSocialSecurityNumber()))
          .ficoScore(customer.getFicoScore())
          .dateOfBirth(customer.getDateOfBirth())
          .firstName(customer.getFirstName())
          .middleName(customer.getMiddleName())
          .lastName(customer.getLastName())
          .addressLine1(customer.getAddressLine1())
          .addressLine2(customer.getAddressLine2())
          // .city(customer.getCity())
          .state(customer.getStateCode())
          .zipCode(customer.getZipCode())
          .country(customer.getCountryCode())
          .phoneNumber1(customer.getPhoneNumber1())
          .phoneNumber2(customer.getPhoneNumber2())
          .governmentId(customer.getGovernmentIssuedId())
          .eftAccountId(customer.getEftAccountId())
          .primaryCardHolderFlag(customer.getPrimaryCardHolderIndicator())
          .infoMessage("Displaying details of given Account")
          .build();

    } catch (Exception e) {
      log.error("Error reading account data for accountId: {}", accountId, e);
      return responseBuilder
          .errorMessage("Error reading account data: " + e.getMessage())
          .inputValid(false)
          .build();
    }
  }

  /**
   * Formatea SSN con guiones - equivalente a STRING en COBOL
   */
  private String formatSsn(String ssn) {
    if (ssn == null || ssn.length() != 9) {
      return ssn;
    }
    return ssn.substring(0, 3) + "-" + ssn.substring(3, 5) + "-" + ssn.substring(5);
  }

  /**
   * Obtiene fecha actual en formato MM/DD/YY - equivalente a WS-CURDATE-MM-DD-YY
   */
  private String getCurrentDate() {
    return LocalDateTime.now().format(DateTimeFormatter.ofPattern("MM/dd/yy"));
  }

  /**
   * Obtiene hora actual en formato HH:MM:SS - equivalente a WS-CURTIME-HH-MM-SS
   */
  private String getCurrentTime() {
    return LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
  }
}
