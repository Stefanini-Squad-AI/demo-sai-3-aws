package com.card.management.Services;

import com.card.management.DTOs.AccountUpdateRequestDto;
import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

@Service
public class AccountValidationService {
  // Patrones de validación equivalentes a las validaciones COBOL
  private static final Pattern PHONE_PATTERN = Pattern.compile("^\\(\\d{3}\\)\\d{3}-\\d{4}$");
  private static final Pattern SSN_PATTERN = Pattern.compile("^\\d{3}-\\d{2}-\\d{4}$");
  private static final Pattern ZIP_PATTERN = Pattern.compile("^\\d{5}$");
  private static final Pattern ALPHA_PATTERN = Pattern.compile("^[A-Za-z\\s]+$");
  private static final Pattern NUMERIC_PATTERN = Pattern.compile("^\\d+$");

  /**
   * Valida los datos de entrada para actualización de cuenta
   * Equivalente a las validaciones en 1200-EDIT-MAP-INPUTS
   */
  public List<String> validateAccountUpdate(AccountUpdateRequestDto request) {
    List<String> errors = new ArrayList<>();

    // Validación de Account ID (equivalente a 1210-EDIT-ACCOUNT)
    if (request.getAccountId() == null) {
      errors.add("Account number must be provided");
    } else if (request.getAccountId().equals(00000000000L)) {
      errors.add("Account number must be a 11 digit non-zero number");
    }

    // Validación de Account Status (equivalente a 1220-EDIT-YESNO)
    if (request.getActiveStatus() == null || request.getActiveStatus().trim().isEmpty()) {
      errors.add("Account Active Status must be supplied");
    } else if (!request.getActiveStatus().matches("[YN]")) {
      errors.add("Account Active Status must be Y or N");
    }

    // Validación de Credit Limit (equivalente a 1250-EDIT-SIGNED-9V2)
    if (request.getCreditLimit() == null) {
      errors.add("Credit Limit must be supplied");
    }

    // Validación de nombres (equivalente a 1225-EDIT-ALPHA-REQD)
    if (request.getFirstName() == null || request.getFirstName().trim().isEmpty()) {
      errors.add("First Name must be supplied");
    } else if (!ALPHA_PATTERN.matcher(request.getFirstName()).matches()) {
      errors.add("First Name can have alphabets only");
    }

    if (request.getLastName() == null || request.getLastName().trim().isEmpty()) {
      errors.add("Last Name must be supplied");
    } else if (!ALPHA_PATTERN.matcher(request.getLastName()).matches()) {
      errors.add("Last Name can have alphabets only");
    }

    // Validación de SSN (equivalente a 1265-EDIT-US-SSN)
    // if (request.getSsn() != null && !request.getSsn().trim().isEmpty()) {
    //   if (!SSN_PATTERN.matcher(request.getSsn()).matches()) {
    //     errors.add("SSN format must be XXX-XX-XXXX");
    //   } else {
    //     String[] ssnParts = request.getSsn().split("-");
    //     String firstPart = ssnParts[0];
    //     // Validación equivalente a INVALID-SSN-PART1
    //     if (firstPart.equals("000") || firstPart.equals("666") ||
    //         (Integer.parseInt(firstPart) >= 900 && Integer.parseInt(firstPart) <= 999)) {
    //       errors.add("SSN: First 3 chars should not be 000, 666, or between 900 and 999");
    //     }
    //   }
    // }

    // Validación de FICO Score (equivalente a 1275-EDIT-FICO-SCORE)
    if (request.getFicoScore() != null) {
      if (request.getFicoScore() < 300 || request.getFicoScore() > 850) {
        errors.add("FICO Score should be between 300 and 850");
      }
    }

    // Validación de dirección requerida
    if (request.getAddressLine1() == null || request.getAddressLine1().trim().isEmpty()) {
      errors.add("Address Line 1 must be supplied");
    }

    // Validación de estado
    if (request.getStateCode() == null || request.getStateCode().trim().isEmpty()) {
      errors.add("State must be supplied");
    } else if (!ALPHA_PATTERN.matcher(request.getStateCode()).matches() ||
        request.getStateCode().length() != 2) {
      errors.add("State must be a 2 character alphabetic code");
    }

    // Validación de código postal
    if (request.getZipCode() == null || request.getZipCode().trim().isEmpty()) {
      errors.add("Zip code must be supplied");
    } else if (!ZIP_PATTERN.matcher(request.getZipCode()).matches()) {
      errors.add("Zip code must be a 5 digit number");
    }

    return errors;
  }

  /**
   * Compara datos antiguos con nuevos para detectar cambios
   * Equivalente a 1205-COMPARE-OLD-NEW
   */
  public boolean hasChanges(AccountUpdateRequestDto oldData, AccountUpdateRequestDto newData) {
    // Implementación de comparación similar a la lógica COBOL
    return !oldData.equals(newData);
  }
}
