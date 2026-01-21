# Card Management System

A modern web application for managing credit card operations, including card issuance, transaction processing, account management, and batch operations. This system was migrated from a legacy COBOL application to a modern Spring Boot + React technology stack.

## 📚 Documentation

Complete technical documentation is available in the `/docs` directory:

| Document | Description | Link |
|----------|-------------|------|
| **System Overview** | Architecture, modules, business processes, and actors | [docs/system-overview.md](docs/system-overview.md) |
| **Deployment Guide** | Local setup, production deployment, and troubleshooting | [docs/deployment.md](docs/deployment.md) |
| **Data Model Reference** | Database schema, entities, relationships, and constraints | [docs/data-model.md](docs/data-model.md) |

## 🚀 Quick Start

### Prerequisites
- Docker 20.10+
- Docker Compose 2.0+

### Start the Application

```bash
# Clone the repository
git clone <repository-url>
cd repo

# Start all services (database, backend, frontend)
docker-compose up -d

# Wait for services to be ready (2-3 minutes)
docker-compose logs -f card-management-app

# Access the application
# Frontend: http://localhost:3000
# Backend API: http://localhost:8080
# Swagger UI: http://localhost:8080/swagger-ui.html
```

### Default Credentials

| Username | Password | Role | Access Level |
|----------|----------|------|--------------|
| admin01 | admin123 | ADMIN | Full system access |
| user01 | user123 | USER | Card and transaction operations |

**⚠️ Change these passwords in production!**

## 🏗️ Architecture

### High-Level Overview

```
┌─────────────────┐         ┌─────────────────┐         ┌─────────────────┐
│  React Frontend │◄───────►│  Spring Boot    │◄───────►│   PostgreSQL    │
│  (TypeScript)   │         │  Backend API    │         │   Database      │
│  Port: 3000     │         │  Port: 8080     │         │   Port: 5432    │
└─────────────────┘         └─────────────────┘         └─────────────────┘
```

### Technology Stack

**Backend**:
- Spring Boot 3.5.6
- Java 21
- Spring Data JPA / Hibernate
- Spring Security + JWT
- Spring Batch
- PostgreSQL 15
- SpringDoc OpenAPI 2.7.0

**Frontend**:
- React 18+
- TypeScript
- React Router 7
- Tailwind CSS
- Vite

**DevOps**:
- Docker & Docker Compose
- Maven
- Nginx (production)

## 📖 API Documentation

### Interactive Documentation (Swagger UI)

Access the interactive API documentation at: **http://localhost:8080/swagger-ui.html**

**How to Use**:
1. Navigate to http://localhost:8080/swagger-ui.html
2. Click the **"Authorize"** button (top right)
3. Login to get an access token:
   ```bash
   curl -X POST http://localhost:8080/api/auth/login \
     -H "Content-Type: application/json" \
     -d '{"userId":"admin01","password":"admin123"}'
   ```
4. Copy the `accessToken` from the response
5. Paste it into the "Value" field (without "Bearer" prefix)
6. Click "Authorize", then "Close"
7. Now you can test all protected endpoints!

### API Groups

The API is organized into functional groups:

1. **Authentication** (`/api/auth/**`)
   - Login, token refresh, token validation
   - Public endpoints (no authentication required)

2. **Users** (`/api/users/**`)
   - User management (ADMIN only)
   - Create, read, update, delete users

3. **Cards** (`/api/cards/**`)
   - Card listing, details, and updates
   - Card status management

4. **Accounts** (`/api/accounts/**`)
   - Account viewing and updates
   - Balance and limit management

5. **Transactions** (`/api/transactions/**`)
   - Transaction processing
   - Transaction history and reports
   - Bill payments

6. **Batch Jobs** (`/api/batch/**`)
   - Batch job execution (ADMIN only)
   - Job status and monitoring

### OpenAPI JSON Specification

Raw OpenAPI 3.0 specification: **http://localhost:8080/v3/api-docs**

Import this into:
- Postman
- Insomnia
- Any OpenAPI-compatible tool

### Example API Calls

**1. Login**:
```bash
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{
    "userId": "admin01",
    "password": "admin123"
  }'
```

Response:
```json
{
  "accessToken": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "refreshToken": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "userId": "admin01",
  "userType": "A",
  "fullName": "Admin User"
}
```

**2. Get All Cards** (authenticated):
```bash
TOKEN="<your-access-token>"

curl -X GET http://localhost:8080/api/cards \
  -H "Authorization: Bearer $TOKEN"
```

**3. Create Transaction**:
```bash
curl -X POST http://localhost:8080/api/transactions \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "cardNumber": "4556737586899855",
    "amount": 99.99,
    "merchantName": "Amazon",
    "transactionType": "PR"
  }'
```

**4. Run Batch Job** (ADMIN only):
```bash
curl -X POST http://localhost:8080/api/batch/transaction-posting \
  -H "Authorization: Bearer $TOKEN"
```

## 🔧 Development

### Local Development Setup

See [Deployment Guide](docs/deployment.md) for complete setup instructions.

**Quick commands**:

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop services
docker-compose down

# Restart backend after code changes
docker-compose restart card-management-app

# Access database
docker-compose exec postgres psql -U card_user -d card_management

# Run batch job manually
docker-compose exec card-management-app \
  curl -X POST http://localhost:8080/api/batch/transaction-posting \
  -H "Authorization: Bearer $TOKEN"
```

### Hot Reload

**Backend**: Source code is mounted for live reload. Changes to Java files trigger automatic recompilation.

**Frontend**: For hot reload, run locally:
```bash
cd frontend
npm install
npm run dev
# Access at http://localhost:5173
```

### Debugging

Backend debug port **5005** is exposed:

**IntelliJ IDEA**: Create Remote JVM Debug configuration pointing to `localhost:5005`

**VS Code**: Use this launch configuration:
```json
{
  "type": "java",
  "name": "Attach to Docker",
  "request": "attach",
  "hostName": "localhost",
  "port": 5005
}
```

## 🎯 Main Features

### 1. Card Management
- Issue new credit/debit cards
- View card details and status
- Update card information
- Block/unblock cards
- Multiple cards per account

### 2. Account Management
- View account balances
- Track credit limits
- Monitor available credit
- Manage cash advance limits
- View cycle credits and debits

### 3. Transaction Processing
- Real-time transaction processing
- Transaction categorization
- Purchase, payment, cash advance, refund, fee, interest
- Merchant information tracking
- Pending authorization management

### 4. Batch Jobs (Automated)
- **Transaction Posting**: Process pending transactions (2:00 AM daily)
- **Interest Calculation**: Calculate and apply interest (3:00 AM daily)
- **Statement Generation**: Generate monthly statements (4:00 AM on 1st)
- **Auth Cleanup**: Remove expired authorizations (1:00 AM daily)

### 5. Security
- JWT-based authentication
- Role-based access control (ADMIN/USER)
- BCrypt password encryption
- Secure API endpoints

### 6. Reporting
- Transaction reports
- Account statements (TEXT/HTML)
- Category-based spending analysis

## 📊 Database

### Core Entities

| Entity | Description | Key Fields |
|--------|-------------|------------|
| **User** | System users with roles | userId, password, userType |
| **Customer** | Customer personal info | customerId, name, address, SSN |
| **Account** | Credit accounts | accountId, balance, creditLimit |
| **Card** | Credit/debit cards | cardNumber, CVV, expirationDate, status |
| **TransactionRecord** | Completed transactions | transactionId, amount, merchant |

See [Data Model Reference](docs/data-model.md) for complete schema.

### Database Access

```bash
# Access PostgreSQL
docker-compose exec postgres psql -U card_user -d card_management

# View tables
\dt

# View users
SELECT * FROM sec_user_data;

# View cards
SELECT * FROM card;

# Exit
\q
```

### Backup & Restore

```bash
# Backup
docker-compose exec postgres pg_dump -U card_user card_management > backup.sql

# Restore
docker-compose exec -T postgres psql -U card_user card_management < backup.sql
```

## 🔒 Security

### Authentication Flow

1. User sends credentials to `POST /api/auth/login`
2. Server validates and generates JWT tokens
3. Client receives `accessToken` and `refreshToken`
4. Client includes token in subsequent requests: `Authorization: Bearer <token>`
5. Server validates token on each request

### Roles

- **ADMIN** (Type 'A'): Full system access including user management and batch jobs
- **USER** (Type 'U'): Limited access to card and transaction operations

### Password Requirements

- Minimum 8 characters
- BCrypt encrypted (60-character hash)
- Never stored in plain text
- Change default passwords in production!

## 🚢 Production Deployment

See [Deployment Guide](docs/deployment.md) for complete production setup.

**Quick production start**:

```bash
# Use production compose file
docker-compose -f docker-composeprod.yml up -d

# Services accessible at:
# Frontend: http://localhost:85
# Backend: http://localhost:8082
# Swagger: http://localhost:8082/swagger-ui.html
```

**Production checklist**:
- [ ] Change database password
- [ ] Generate strong JWT secret
- [ ] Change default user passwords
- [ ] Disable schema auto-update
- [ ] Configure SSL/HTTPS
- [ ] Set up backups
- [ ] Configure monitoring
- [ ] Review firewall rules

## 🧪 Testing

### Test Authentication

```bash
# Health check
curl http://localhost:8080/api/auth/health

# Login test
curl -X POST http://localhost:8080/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"userId":"admin01","password":"admin123"}'

# Should return tokens
```

### Postman Collection

Import batch jobs collection:
- File: `management/postman/batch-jobs-collection.json`
- Contains pre-configured requests for all batch jobs

## 📝 Logging

**View logs**:
```bash
# All services
docker-compose logs -f

# Backend only
docker-compose logs -f card-management-app

# Filter by level
docker-compose logs card-management-app | grep ERROR
```

**Log file location** (inside container):
```
/app/logs/card-management.log
```

**Log levels**:
- `DEBUG`: Detailed application flow
- `INFO`: General information
- `WARN`: Warning messages
- `ERROR`: Error conditions

## 🔍 Troubleshooting

### Common Issues

**1. Database connection failed**
```bash
# Check database is running
docker-compose ps postgres

# View database logs
docker-compose logs postgres

# Restart database
docker-compose restart postgres
```

**2. Backend won't start**
```bash
# Check logs
docker-compose logs card-management-app

# Check port availability
lsof -i :8080

# Rebuild
docker-compose down
docker-compose build --no-cache card-management-app
docker-compose up -d
```

**3. Swagger UI 404**
- Wait for backend to fully start (check logs)
- Try: http://localhost:8080/swagger-ui/index.html
- Verify SpringDoc dependency in pom.xml

**4. Authentication fails**
- Verify user exists in database
- Check password (case-sensitive)
- Ensure JWT secret is configured

See [Deployment Guide - Troubleshooting](docs/deployment.md#troubleshooting) for more solutions.

## 📚 Additional Resources

### Documentation
- [System Overview](docs/system-overview.md) - Architecture and business processes
- [Deployment Guide](docs/deployment.md) - Setup and operations
- [Data Model](docs/data-model.md) - Database schema and entities

### API Documentation
- [Swagger UI](http://localhost:8080/swagger-ui.html) - Interactive API docs
- [OpenAPI Spec](http://localhost:8080/v3/api-docs) - Raw specification

### Tools
- [Postman Collection](management/postman/batch-jobs-collection.json) - Batch jobs
- [Frontend README](frontend/README.md) - React app documentation

## 🏢 System Context

### Business Processes

**Card Issuance**:
1. Customer account created
2. Card issued with unique number, CVV, expiration
3. Card linked to account
4. Card activated (status = 'A')

**Transaction Processing**:
1. Transaction initiated (POS/online/ATM)
2. Card and credit validated
3. Pending authorization created
4. Transaction posted (immediate or batch)
5. Account balance updated

**Batch Operations**:
1. Transaction Posting: Process daily transactions
2. Interest Calculation: Apply interest charges
3. Statement Generation: Monthly account statements
4. Auth Cleanup: Remove expired authorizations

See [System Overview - Key Business Processes](docs/system-overview.md#key-business-processes) for details.

## 🤝 Contributing

### Development Workflow

1. Create feature branch
2. Make changes (hot reload active)
3. Test locally
4. Submit pull request

### Code Style

- **Java**: Follow Spring Boot conventions
- **TypeScript**: ESLint + Prettier
- **SQL**: Uppercase keywords, snake_case tables

### Commit Messages

```
feat: Add new transaction report endpoint
fix: Resolve JWT expiration issue
docs: Update API documentation
refactor: Simplify batch job configuration
```

## 📄 License

Copyright Amazon.com, Inc. or its affiliates.  
Licensed under the Apache License, Version 2.0

See LICENSE file for details.

## 🆘 Support

### Getting Help

1. Check documentation in `/docs`
2. Review Swagger UI for API reference
3. Check logs for error messages
4. Search existing issues
5. Contact development team

### Reporting Bugs

Include:
- Steps to reproduce
- Expected vs actual behavior
- Logs and error messages
- Environment details (Docker version, OS)
- API request/response (if applicable)

## 🗺️ Roadmap

### Completed ✅
- [x] COBOL to Spring Boot migration
- [x] JWT authentication
- [x] Core CRUD operations
- [x] Batch job framework
- [x] API documentation (Swagger)
- [x] Docker deployment
- [x] Comprehensive technical documentation

### Planned 🚧
- [ ] Additional unit tests
- [ ] Integration tests
- [ ] Performance optimization
- [ ] Enhanced monitoring
- [ ] GraphQL API option
- [ ] Mobile app support

---

**Need Help?**  
📖 Start with the [System Overview](docs/system-overview.md)  
🚀 Follow the [Deployment Guide](docs/deployment.md)  
🔍 Explore the [Data Model](docs/data-model.md)  
📡 Try the [Swagger UI](http://localhost:8080/swagger-ui.html)
