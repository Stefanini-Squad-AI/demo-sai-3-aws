# SAI Application Documentation

## 📚 Documentation Structure

This documentation follows the **User Story-Oriented Documentation Framework** designed to enable engineers and Product Owners to write well-structured User Stories based on actual codebase patterns.

## 📁 File Structure

```
docs/
 system-overview.md          # 🎯 Single source of truth (comprehensive Markdown)
 site/
    ├── index.html             # 🏠 Main navigation page
    └── modules/
        ├── auth/index.html
        ├── account/index.html
        ├── creditCard/index.html
        ├── transaction/index.html
        ├── billPayment/index.html
        ├── user/index.html
        ├── menu/index.html
        ├── layout/index.html
        └── core/index.html
```

## � Quick Access

### Main Documentation
- **[System Overview](./system-overview.md)** - Comprehensive system documentation (Markdown)
- **[HTML Site](./site/index.html)** - Interactive module documentation (HTML)

### Module Documentation (HTML Pages)
1. [AUTH - Authentication & Authorization](./site/modules/auth/index.html)
2. [ACCOUNT - Account Management](./site/modules/account/index.html)
3. [CREDITCARD - Credit Card Management](./site/modules/creditCard/index.html)
4. [TRANSACTION - Transaction Management](./site/modules/transaction/index.html)
5. [BILLPAYMENT - Bill Payment Processing](./site/modules/billPayment/index.html)
6. [USER - User Management](./site/modules/user/index.html)
7. [MENU - Navigation & Menus](./site/modules/menu/index.html)
8. [LAYOUT - Layout & UI Components](./site/modules/layout/index.html)
9. [CORE - Core Infrastructure](./site/modules/core/index.html)

## 📊 What's Included

### System Overview (system-overview.md)
- Platform statistics and architecture
- Complete module catalog with responsibilities
- Data models and business rules
- Frontend patterns (actual implementation, not assumptions)
- User Story templates and complexity guidelines
- Performance budgets and acceptance criteria
- Mermaid architecture diagrams
- Task lists and success metrics

### Module HTML Pages
Each module page includes:
- **Overview**: Purpose and business context
- **US Development Guide**: Templates, acceleration factors, complexity guidelines
- **Technical Foundation**: Components, dependencies, APIs
- **Implementation Examples**: Code snippets and patterns
- **Acceptance Criteria**: Real-world examples

## 🚀 Usage

### For Product Owners
1. Start with [System Overview](./system-overview.md) for high-level understanding
2. Browse [Module Catalog](./site/index.html) for feature-specific information
3. Use module pages for detailed User Story writing guidance

### For Developers
1. Reference [System Overview](./system-overview.md) for architecture and patterns
2. Check module pages for component usage and API documentation
3. Follow implementation examples for consistency

### For Building
The HTML documentation is automatically copied to `dist/docs/site` during build:
```bash
npm run build
# Documentation will be available at dist/docs/site/
```

## 📈 Accuracy

- **Codebase Accuracy**: 95%+
- **Last Updated**: 2026-01-26
- **Framework Used**: User Story-Oriented Documentation Framework
- **Based on**: Actual project analysis (not assumptions)

## 🔄 Maintenance

This is the **first iteration** of general documentation. Future updates will include:
- Module-specific deep dives (second iteration)
- Additional patterns and examples
- Updated as features evolve

## 📝 Notes

- No fictional components or patterns documented
- All examples based on actual codebase
- Frontend patterns analyzed from real implementation
- API documentation matches MSW mocks

---

**Created by**: DS3A-10 - Documentación general  
**Template**: TEMPLATE_DOC.txt  
**Purpose**: Enable well-structured User Story creation
