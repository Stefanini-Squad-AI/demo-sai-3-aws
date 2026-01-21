package com.card.management.Utils;

import org.springframework.stereotype.Component;

/**
 * Utilidades de validación
 * Equivalente a las validaciones en 2210-EDIT-ACCOUNT
 */
@Component
public class ValidationUtil {
  /**
   * Valida que el ID de cuenta sea un número de 11 dígitos no cero
   * Equivalente a las validaciones en 2210-EDIT-ACCOUNT
   */
  public boolean isValidAccountId(Long accountId) {
    if (accountId == null) {
      return false;
    }

    // Debe tener 11 dígitos
    if (accountId.toString().length() != 11) {
      return false;
    }

    // No debe ser cero
    if (accountId.equals(00000000000L)) {
      return false;
    }

    return true;
  }
}
