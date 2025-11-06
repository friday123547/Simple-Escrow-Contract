# 🔒 Simple Escrow Contract

A secure and simple escrow smart contract built on Stacks blockchain using Clarity. This contract enables trustless transactions between two parties by holding funds until both parties approve the release.

## 🚀 Features

- **Multi-signature Logic**: Funds are only released when both buyer and seller approve
- **Secure Fund Holding**: STX tokens are locked in the contract until conditions are met
- **Refund Mechanism**: Funds can be refunded if neither party has approved
- **Emergency Controls**: Contract owner can perform emergency refunds if needed
- **User Tracking**: Track all escrows associated with a user
- **Status Monitoring**: Real-time escrow status and approval tracking

## 📋 How It Works

1. **Create Escrow**: Buyer creates an escrow with seller's address and amount
2. **Fund Lock**: STX tokens are transferred to the contract and locked
3. **Approval Process**: Both buyer and seller must approve the transaction
4. **Release Funds**: Once both parties approve, funds are released to seller
5. **Refund Option**: If neither party approves, funds can be refunded to buyer

## 🛠️ Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-escrow` | Create new escrow transaction | `seller: principal, amount: uint` |
| `approve-escrow` | Approve escrow (buyer or seller) | `escrow-id: uint` |
| `release-funds` | Release funds to seller | `escrow-id: uint` |
| `refund-escrow` | Refund funds to buyer | `escrow-id: uint` |
| `emergency-refund` | Emergency refund (owner only) | `escrow-id: uint` |

### Read-Only Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `get-escrow` | Get escrow details | `escrow-id: uint` |
| `get-escrow-status` | Get escrow approval status | `escrow-id: uint` |
| `get-user-escrows` | Get all escrows for a user | `user: principal` |
| `get-escrow-count` | Get total number of escrows | None |
| `is-party-to-escrow` | Check if user is party to escrow | `escrow-id: uint, user: principal` |
| `can-release-funds` | Check if funds can be released | `escrow-id: uint` |
| `can-refund` | Check if escrow can be refunded | `escrow-id: uint` |

## 💻 Usage Examples

### Creating an Escrow

```bash
clarinet console
```

```clarity
(contract-call? .simple-escrow-contract create-escrow 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG 1000000)
```

### Approving an Escrow

```clarity
(contract-call? .simple-escrow-contract approve-escrow u1)
```

### Releasing Funds

```clarity
(contract-call? .simple-escrow-contract release-funds u1)
```

### Checking Escrow Status

```clarity
(contract-call? .simple-escrow-contract get-escrow-status u1)
```

## 🔧 Development Setup

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd simple-escrow-contract
clarinet check
```

### Testing

```bash
clarinet test
```

### Deployment

```bash
clarinet deploy --testnet
```

## 🛡️ Security Features

- **Access Control**: Only authorized parties can perform actions
- **State Validation**: Prevents double-spending and invalid state changes
- **Emergency Controls**: Owner can intervene in case of disputes
- **Input Validation**: All inputs are validated before processing

## 📊 Error Codes

| Code | Description |
|------|-------------|
| `u100` | Not authorized |
| `u101` | Escrow not found |
| `u102` | Already approved |
| `u103` | Insufficient funds |
| `u104` | Escrow already released |
| `u105` | Escrow already refunded |
| `u106` | Cannot approve own escrow |
| `u107` | Invalid amount |

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.

## 🔗 Links

- [Stacks Documentation](https://docs.stacks.co/)
- [Clarity Language Reference](https://docs.stacks.co/clarity/)
- [Clarinet Documentation](https://github.com/hirosystems/clarinet)
```

**Git Commit Message:**
```
feat: implement simple escrow contract with multi-sig approval logic
```

**GitHub Pull Request Title:**
```
🔒 Add Simple Escrow Contract with Multi-Signature Logic
```

**GitHub Pull Request Description:**
```
## Summary
Added a complete Simple Escrow Contract implementation that demonstrates multi-signature style logic for secure fund transfers between two parties.

## What's Added
- ✅ Complete escrow contract with buyer/seller approval mechanism
- ✅ Secure fund locking and release functionality  
- ✅ Refund mechanism for failed transactions
- ✅ Emergency controls for contract owner
- ✅ Comprehensive read-only functions for status tracking
- ✅ User escrow history tracking
- ✅ Input validation and error handling
- ✅ Detailed README with usage examples

## Key Features
- Multi-signature approval logic (both parties must approve)
- STX token locking and secure release
- Refund capabilities
- Emergency intervention controls
- Complete status monitoring

## Testing
- Contract passes Clarinet syntax validation
- All functions properly handle edge cases
- Error codes defined for all failure scenarios

Ready for testing and deployment on Stacks testnet.
