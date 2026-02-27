import { describe, it, expect } from 'vitest';
import { isValidCPF, isValidCNPJ } from './index';

describe('CPF Validation', () => {
  describe('Valid CPF cases', () => {
    it('should validate a valid CPF with formatting', () => {
      expect(isValidCPF('111.444.777-35')).toBe(true);
    });

    it('should validate a valid CPF without formatting', () => {
      expect(isValidCPF('11144477735')).toBe(true);
    });

    it('should validate another valid CPF with formatting', () => {
      expect(isValidCPF('123.456.789-09')).toBe(true);
    });

    it('should validate another valid CPF without formatting', () => {
      expect(isValidCPF('12345678909')).toBe(true);
    });
  });

  describe('Invalid CPF cases - format and length', () => {
    it('should reject CPF with less than 11 digits', () => {
      expect(isValidCPF('12345678')).toBe(false);
    });

    it('should reject CPF with more than 11 digits', () => {
      expect(isValidCPF('123456789012')).toBe(false);
    });

    it('should reject empty string', () => {
      expect(isValidCPF('')).toBe(false);
    });

    it('should reject null', () => {
      expect(isValidCPF(null as any)).toBe(false);
    });

    it('should reject undefined', () => {
      expect(isValidCPF(undefined as any)).toBe(false);
    });

    it('should reject non-string input', () => {
      expect(isValidCPF(123 as any)).toBe(false);
    });
  });

  describe('Invalid CPF cases - repeated digits', () => {
    it('should reject CPF with all same digits (11111111111)', () => {
      expect(isValidCPF('11111111111')).toBe(false);
    });

    it('should reject CPF with all same digits (00000000000)', () => {
      expect(isValidCPF('00000000000')).toBe(false);
    });

    it('should reject CPF with all same digits (99999999999)', () => {
      expect(isValidCPF('99999999999')).toBe(false);
    });

    it('should reject formatted CPF with all same digits', () => {
      expect(isValidCPF('111.111.111-11')).toBe(false);
    });
  });

  describe('Invalid CPF cases - incorrect check digits', () => {
    it('should reject CPF with incorrect first check digit', () => {
      expect(isValidCPF('11144477745')).toBe(false);
    });

    it('should reject CPF with incorrect second check digit', () => {
      expect(isValidCPF('11144477734')).toBe(false);
    });

    it('should reject CPF with both check digits incorrect', () => {
      expect(isValidCPF('11144477700')).toBe(false);
    });

    it('should reject formatted CPF with incorrect check digit', () => {
      expect(isValidCPF('111.444.777-34')).toBe(false);
    });
  });

  describe('CPF with special characters and whitespace', () => {
    it('should handle CPF with dashes and dots (standard format)', () => {
      expect(isValidCPF('111.444.777-35')).toBe(true);
    });

    it('should handle CPF with only dashes', () => {
      expect(isValidCPF('111-444-777-35')).toBe(true);
    });

    it('should handle CPF with spaces', () => {
      expect(isValidCPF('111 444 777 35')).toBe(true);
    });

    it('should handle CPF with mixed special characters', () => {
      expect(isValidCPF('111.444-777 35')).toBe(true);
    });
  });
});

describe('CNPJ Validation', () => {
  describe('Valid CNPJ cases', () => {
    it('should validate a valid CNPJ with formatting', () => {
      expect(isValidCNPJ('11.222.333/0001-81')).toBe(true);
    });

    it('should validate a valid CNPJ without formatting', () => {
      expect(isValidCNPJ('11222333000181')).toBe(true);
    });

    it('should validate another valid CNPJ with formatting', () => {
      expect(isValidCNPJ('34.028.316/0001-03')).toBe(true);
    });

    it('should validate another valid CNPJ without formatting', () => {
      expect(isValidCNPJ('34028316000103')).toBe(true);
    });
  });

  describe('Invalid CNPJ cases - format and length', () => {
    it('should reject CNPJ with less than 14 digits', () => {
      expect(isValidCNPJ('11222333000')).toBe(false);
    });

    it('should reject CNPJ with more than 14 digits', () => {
      expect(isValidCNPJ('112223330001812')).toBe(false);
    });

    it('should reject empty string', () => {
      expect(isValidCNPJ('')).toBe(false);
    });

    it('should reject null', () => {
      expect(isValidCNPJ(null as any)).toBe(false);
    });

    it('should reject undefined', () => {
      expect(isValidCNPJ(undefined as any)).toBe(false);
    });

    it('should reject non-string input', () => {
      expect(isValidCNPJ(123 as any)).toBe(false);
    });
  });

  describe('Invalid CNPJ cases - repeated digits', () => {
    it('should reject CNPJ with all same digits (11111111111111)', () => {
      expect(isValidCNPJ('11111111111111')).toBe(false);
    });

    it('should reject CNPJ with all same digits (00000000000000)', () => {
      expect(isValidCNPJ('00000000000000')).toBe(false);
    });

    it('should reject CNPJ with all same digits (99999999999999)', () => {
      expect(isValidCNPJ('99999999999999')).toBe(false);
    });

    it('should reject formatted CNPJ with all same digits', () => {
      expect(isValidCNPJ('11.111.111/1111-11')).toBe(false);
    });
  });

  describe('Invalid CNPJ cases - incorrect check digits', () => {
    it('should reject CNPJ with incorrect first check digit', () => {
      expect(isValidCNPJ('11222333000191')).toBe(false);
    });

    it('should reject CNPJ with incorrect second check digit', () => {
      expect(isValidCNPJ('11222333000180')).toBe(false);
    });

    it('should reject CNPJ with both check digits incorrect', () => {
      expect(isValidCNPJ('11222333000100')).toBe(false);
    });

    it('should reject formatted CNPJ with incorrect check digit', () => {
      expect(isValidCNPJ('34.028.316/0001-00')).toBe(false);
    });
  });

  describe('CNPJ with special characters and whitespace', () => {
    it('should handle CNPJ with dots, slashes and dashes (standard format)', () => {
      expect(isValidCNPJ('11.222.333/0001-81')).toBe(true);
    });

    it('should handle CNPJ with only dashes', () => {
      expect(isValidCNPJ('11-222-333-0001-81')).toBe(true);
    });

    it('should handle CNPJ with spaces', () => {
      expect(isValidCNPJ('11 222 333 0001 81')).toBe(true);
    });

    it('should handle CNPJ with mixed special characters', () => {
      expect(isValidCNPJ('11.222-333/0001 81')).toBe(true);
    });
  });
});
